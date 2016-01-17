{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Sound where
import PreludeExtra

import Rumpus.Types
import Rumpus.Systems.Shared

import qualified Data.Map as Map

createSoundSystem :: MonadIO m => PureData -> m ()
createSoundSystem pd = do
    mapM_ (addToLibPdSearchPath pd)
        ["resources/pd-kit", "resources/pd-kit/list-abs"]

openALSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => M44 GLfloat -> m ()
openALSystem headM44 = do
    -- Update souce and listener poitions
    alListenerPose (poseFromMatrix headM44)
    mapM_ (\(entityID, sourceID) -> do
        position <- view posPosition <$> getEntityPose entityID
        alSourcePosition sourceID position)
        =<< Map.toList <$> use (wldComponents . cmpSoundSource)

dequeueOpenALSource :: MonadState World m => m (Maybe (Int, OpenALSource))
dequeueOpenALSource = do
    sources <- use wldOpenALSourcePool
    case sources of
        [] -> return Nothing
        (x:xs) -> do
            wldOpenALSourcePool .= xs ++ [x]
            return (Just x)

addPdPatchComponent :: (MonadReader WorldStatic m, MonadState World m, MonadIO m) => EntityID -> Entity -> m ()
addPdPatchComponent entityID entity = forM_ (entity ^. entPdPatch) $ \patchPath -> do
    pd <- view wlsPd
    patch <- makePatch pd patchPath
    wldComponents . cmpPdPatch . at entityID ?= patch
    -- Assign the patch's output DAC index to route it to the the SourceID
    dequeueOpenALSource >>= mapM_ (\(sourceChannel, _sourceID) -> do
        send pd patch "dac" (Atom (Float (fromIntegral sourceChannel)))
        )

removePdPatchComponent :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => EntityID -> m ()
removePdPatchComponent entityID = do
    pd <- view wlsPd
    withPdPatch entityID $ \patch ->
        closePatch pd patch
    wldComponents . cmpPdPatch . at entityID .= Nothing

withPdPatch :: MonadState World m => EntityID -> (Patch -> m b) -> m ()
withPdPatch entityID = useMaybeM_ (wldComponents . cmpPdPatch . at entityID)

sendPd :: (MonadIO m, MonadReader WorldStatic m) => Patch -> Receiver -> Message -> m ()
sendPd patch receiver message = do
    pd <- view wlsPd
    send pd patch receiver message
