{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Sound where
import PreludeExtra

import Rumpus.Systems.Shared
import Data.ECS


data SoundSystem = SoundSystem 
    { _sdsPd               :: !PureData
    , _sdsOpenALSourcePool :: ![(Int, OpenALSource)]
    }
makeLenses ''SoundSystem
defineSystemKey ''SoundSystem

defineComponentKeyWithType "PdPatch" [t|Patch|]
defineComponentKeyWithType "PdPatchFile" [t|FilePath|]
defineComponentKey ''OpenALSource

initSoundSystem :: (MonadState ECS m, MonadIO m) => PureData -> m ()
initSoundSystem pd = do
    mapM_ (addToLibPdSearchPath pd)
        ["resources/pd-kit", "resources/pd-kit/list-abs"]

    let soundSystem = SoundSystem { _sdsPd = pd, _sdsOpenALSourcePool = zip [1..] (pdSources pd) }

    registerSystem sysSound soundSystem

    registerComponent "PdPatchFile" cmpPdPatchFile (newComponentInterface cmpPdPatchFile)
    registerComponent "OpenALSource" cmpOpenALSource (newComponentInterface cmpOpenALSource)
    registerComponent "PdPatch" cmpPdPatch $ (newComponentInterface cmpPdPatch)
        { ciDeriveComponent = Just (derivePdPatchComponent pd) 
        , ciRemoveComponent = removePdPatchComponent }

tickSoundSystem :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m ()
tickSoundSystem headM44 = do
    -- Update souce and listener poitions
    alListenerPose (poseFromMatrix headM44)
    forEntitiesWithComponent cmpOpenALSource $ \(entityID, sourceID) -> do
        position <- view posPosition <$> getEntityPose entityID
        alSourcePosition sourceID position

dequeueOpenALSource :: MonadState ECS m => m (Maybe (Int, OpenALSource))
dequeueOpenALSource = modifySystemState sysSound $ do
    openALSourcePool <- use sdsOpenALSourcePool
    case openALSourcePool of
        [] -> return Nothing
        (x:xs) -> do
            sdsOpenALSourcePool .= xs ++ [x]
            return (Just x)

derivePdPatchComponent :: (MonadReader EntityID m, MonadState ECS m, MonadIO m) => PureData -> m ()
derivePdPatchComponent pd = 
    withComponent_ cmpPdPatchFile $ \patchFile -> do
        patch <- makePatch pd patchFile
        cmpPdPatch ==> patch

        -- Assign the patch's output DAC index to route it to the the SourceID
        traverseM_ dequeueOpenALSource $ \(sourceChannel, sourceID) -> do
            putStrLnIO $ "loaded " ++ patchFile ++ " sending " ++ show sourceChannel
            send pd patch "dac" $ Atom (fromIntegral sourceChannel)
            cmpOpenALSource ==> sourceID

removePdPatchComponent :: (MonadReader EntityID m, MonadIO m, MonadState ECS m) => m ()
removePdPatchComponent = do
    pd <- viewSystem sysSound sdsPd
    _ <- withPdPatch $ closePatch pd

    removeComponent cmpPdPatch
    removeComponent cmpOpenALSource

withPdPatch :: (MonadReader EntityID m, MonadState ECS m) => (Patch -> m b) -> m (Maybe b)
withPdPatch = withComponent cmpPdPatch


withEntityPdPatch :: (HasComponents s, MonadState s m) => EntityID -> (Patch -> m b) -> m (Maybe b)
withEntityPdPatch entityID = withEntityComponent entityID cmpPdPatch

sendToPdPatch :: (MonadIO m, MonadState ECS m) => Patch -> Receiver -> Message -> m ()
sendToPdPatch patch receiver message = withSystem_ sysSound $ \soundSystem -> 
    send (soundSystem ^. sdsPd) patch receiver message

sendEntityPdPatch :: (MonadIO m, MonadState ECS m) => EntityID -> Receiver -> Message -> m ()
sendEntityPdPatch entityID receiver message = 
    void . withEntityPdPatch entityID $ \patch -> 
        sendToPdPatch patch receiver message

sendPd :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => Receiver -> Message -> m ()
sendPd receiver message = 
    void . withPdPatch $ \patch -> 
        sendToPdPatch patch receiver message


readPdArray :: (MonadReader EntityID m, MonadIO m, MonadState ECS m, Integral a) => Receiver -> a -> a -> m [Double]
readPdArray arrayName offset count = do
    pd <- viewSystem sysSound sdsPd
    fromMaybe [] . join <$> withPdPatch (\patch ->
        readArray pd (local patch arrayName) offset count)
