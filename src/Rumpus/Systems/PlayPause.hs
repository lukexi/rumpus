{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.PlayPause where
import PreludeExtra

import Data.ECS

data PlayPauseSystem = PlayPauseSystem
    { _plyPlaying :: !Bool
    }
makeLenses ''PlayPauseSystem
defineSystemKey ''PlayPauseSystem

initPlayPauseSystem :: MonadState ECS m => m ()
initPlayPauseSystem = do
    registerSystem sysPlayPause (PlayPauseSystem False)


toggleWorldPlaying :: (MonadState ECS m) => m ()
toggleWorldPlaying = modifySystemState sysPlayPause (plyPlaying %= not)

whenWorldPlaying :: MonadState ECS m => m () -> m ()
whenWorldPlaying action = do
    isPlaying <- viewSystem sysPlayPause plyPlaying
    when isPlaying action
