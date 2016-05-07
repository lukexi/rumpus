{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Animation
    ( module Rumpus.Systems.Animation
    , module Exports
    ) where

import PreludeExtra
import Animation.Pal as Exports hiding (getNow, exhaustTChan)
import Rumpus.Systems.Shared
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Physics
import Rumpus.Systems.Controls

defineComponentKeyWithType "ColorAnimation" [t|Animation (V4 GLfloat)|]
defineComponentKeyWithType "SizeAnimation"  [t|Animation (V3 GLfloat)|]
-- defineComponentKeyWithType "PoseAnimation"  [t|Animation (Pose GLfloat)|]

initAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
initAnimationSystem = do
    registerComponent "ColorAnimation" myColorAnimation (newComponentInterface myColorAnimation)
    registerComponent "SizeAnimation"  mySizeAnimation  (newComponentInterface mySizeAnimation)
    -- registerComponent "PoseAnimation"  myPoseAnimation  (newComponentInterface myPoseAnimation)

tickAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
tickAnimationSystem = whenWorldPlaying $ do
    now <- realToFrac <$> getNow

    tickComponentAnimation now myColorAnimation setColor
    tickComponentAnimation now mySizeAnimation  setSize
    -- tickComponentAnimation now myPoseAnimation  myPose

tickComponentAnimation :: MonadState ECS m
                       => DiffTime
                       -> Key (EntityMap (Animation struct))
                       -> (struct -> ReaderT EntityID m a)
                       -> m ()
tickComponentAnimation now animComponentKey setter =
    forEntitiesWithComponent animComponentKey $
        \(entityID, animation) -> runEntity entityID $ do
            let evaled = evalAnim now animation

            _ <- setter (evanResult evaled)
            when (evanRunning evaled == False) $ do
                removeComponent animComponentKey


animateSizeTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> DiffTime -> m ()
animateSizeTo newSize time = do
    currentSize <- getSize
    animation <- makeAnimation time currentSize newSize
    mySizeAnimation ==> animation
