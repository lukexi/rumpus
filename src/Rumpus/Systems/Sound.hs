{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Sound where
import Rumpus.Types
import Sound.Pd
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Linear.Extra
import Control.Lens.Extra
import Graphics.GL.Pal
import Rumpus.Systems.Shared

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
