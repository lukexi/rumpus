{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Sound where
import PreludeExtra

import Rumpus.Types
import Rumpus.Systems.Shared
import Rumpus.ECS


data SoundSystem = SoundSystem 
    { _sdsPd               :: !PureData
    , _sdsOpenALSourcePool :: ![(Int, OpenALSource)]
    }
makeLenses ''SoundSystem
defineSystemKey ''SoundSystem

defineComponentKeyWithType "PdPatch" [t|Patch|]
defineComponentKey ''OpenALSource



createSoundSystem :: MonadIO m => PureData -> m ()
createSoundSystem pd = do
    mapM_ (addToLibPdSearchPath pd)
        ["resources/pd-kit", "resources/pd-kit/list-abs"]

    let soundSystem = SoundSystem { _sdsPd = pd, _sdsOpenALSourcePool = zip [1..] (pdSources pd) }
    return ()

tickSoundSystem :: (MonadIO m, MonadState World m) => M44 GLfloat -> m ()
tickSoundSystem headM44 = do
    -- Update souce and listener poitions
    alListenerPose (poseFromMatrix headM44)
    forEntitiesWithComponent openALSourceKey $ \(entityID, sourceID) -> do
        position <- view posPosition <$> getEntityPose entityID
        alSourcePosition sourceID position

dequeueOpenALSource :: MonadState World m => m (Maybe (Int, OpenALSource))
dequeueOpenALSource = modifySystemState soundSystemKey $ do
    openALSourcePool <- use sdsOpenALSourcePool
    case openALSourcePool of
        [] -> return Nothing
        (x:xs) -> do
            sdsOpenALSourcePool .= xs ++ [x]
            return (Just x)

addPdPatchComponent :: (MonadState World m, MonadIO m) => EntityID -> FilePath -> m ()
addPdPatchComponent entityID patchPath = withSystem_ soundSystemKey $ \soundSystem -> do
    let pd = soundSystem ^. sdsPd
    patch <- makePatch pd patchPath
    addComponent pdPatchKey patch entityID

    -- Assign the patch's output DAC index to route it to the the SourceID
    traverseM_ dequeueOpenALSource $ \(sourceChannel, sourceID) -> do
        send pd patch "dac" $ Atom (fromIntegral sourceChannel)
        addComponent openALSourceKey sourceID entityID

removePdPatchComponent :: (MonadIO m, MonadState World m) => EntityID -> m ()
removePdPatchComponent entityID = withSystem_ soundSystemKey $ \soundSystem -> do
    let pd = soundSystem ^. sdsPd
    withPdPatch entityID $ \patch ->
        closePatch pd patch

    removeComponentFromEntity pdPatchKey entityID
    removeComponentFromEntity openALSourceKey entityID

withPdPatch :: MonadState World m => EntityID -> (Patch -> m b) -> m ()
withPdPatch entityID = withComponent entityID pdPatchKey

sendPd :: (MonadIO m, MonadState World m) => Patch -> Receiver -> Message -> m ()
sendPd patch receiver message = withSystem_ soundSystemKey $ \soundSystem -> 
    send (soundSystem ^. sdsPd) patch receiver message
