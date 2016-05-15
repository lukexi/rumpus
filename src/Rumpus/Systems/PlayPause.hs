{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.PlayPause where
import PreludeExtra

data PlayPauseSystem = PlayPauseSystem
    { _plyPlaying :: !Bool
    , _plyScriptsEnabled :: !Bool
    }
makeLenses ''PlayPauseSystem
defineSystemKey ''PlayPauseSystem

initPlayPauseSystem :: MonadState ECS m => m ()
initPlayPauseSystem = do
    registerSystem sysPlayPause (PlayPauseSystem False True)


toggleWorldPlaying :: (MonadState ECS m) => m ()
toggleWorldPlaying = modifySystemState sysPlayPause (plyPlaying %= not)

setWorldPlaying :: (MonadState ECS m) => Bool -> m ()
setWorldPlaying playing = modifySystemState sysPlayPause (plyPlaying .= playing)

getWorldPlaying :: MonadState ECS m => m Bool
getWorldPlaying = viewSystem sysPlayPause plyPlaying

whenWorldPlaying :: MonadState ECS m => m () -> m ()
whenWorldPlaying action = do
    isPlaying <- getWorldPlaying
    when isPlaying action


toggleScriptsEnabled :: (MonadState ECS m) => m ()
toggleScriptsEnabled = modifySystemState sysPlayPause (plyScriptsEnabled %= not)

setScriptsEnabled :: (MonadState ECS m) => Bool -> m ()
setScriptsEnabled enabled = modifySystemState sysPlayPause (plyScriptsEnabled .= enabled)

getScriptsEnabled :: MonadState ECS m => m Bool
getScriptsEnabled = viewSystem sysPlayPause plyScriptsEnabled

whenScriptsEnabled :: MonadState ECS m => m () -> m ()
whenScriptsEnabled action = do
    enabled <- getScriptsEnabled
    when enabled action
