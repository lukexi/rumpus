{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Sound where
import PreludeExtra

import Rumpus.Types
import Rumpus.Systems.Shared
import Data.ECS


data SoundSystem = SoundSystem 
    { _sdsPd               :: !PureData
    , _sdsOpenALSourcePool :: ![(Int, OpenALSource)]
    }
makeLenses ''SoundSystem
defineSystemKey ''SoundSystem

defineComponentKeyWithType "PdPatch" [t|Patch|]
defineComponentKey ''OpenALSource



initSoundSystem :: (MonadState ECS m, MonadIO m) => PureData -> m ()
initSoundSystem pd = do
    mapM_ (addToLibPdSearchPath pd)
        ["resources/pd-kit", "resources/pd-kit/list-abs"]

    let soundSystem = SoundSystem { _sdsPd = pd, _sdsOpenALSourcePool = zip [1..] (pdSources pd) }

    registerSystem sysSound soundSystem

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

addPdPatchComponent :: (MonadState ECS m, MonadIO m) => EntityID -> FilePath -> m ()
addPdPatchComponent entityID patchPath = withSystem_ sysSound $ \soundSystem -> do
    let pd = soundSystem ^. sdsPd
    patch <- makePatch pd patchPath
    addComponent cmpPdPatch patch entityID

    -- Assign the patch's output DAC index to route it to the the SourceID
    traverseM_ dequeueOpenALSource $ \(sourceChannel, sourceID) -> do
        send pd patch "dac" $ Atom (fromIntegral sourceChannel)
        addComponent cmpOpenALSource sourceID entityID

removePdPatchComponent :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
removePdPatchComponent entityID = withSystem_ sysSound $ \soundSystem -> do
    let pd = soundSystem ^. sdsPd
    withPdPatch entityID $ \patch ->
        closePatch pd patch

    removeComponent cmpPdPatch entityID
    removeComponent cmpOpenALSource entityID

withPdPatch :: MonadState ECS m => EntityID -> (Patch -> m b) -> m ()
withPdPatch entityID = withComponent entityID cmpPdPatch

sendPd :: (MonadIO m, MonadState ECS m) => Patch -> Receiver -> Message -> m ()
sendPd patch receiver message = withSystem_ sysSound $ \soundSystem -> 
    send (soundSystem ^. sdsPd) patch receiver message
