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
defineComponentKeyWithType "PositionAnimation"  [t|Animation (V3 GLfloat)|]
defineComponentKeyWithType "RotationAnimation"  [t|Animation (Quaternion GLfloat)|]

initAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
initAnimationSystem = do
    registerComponent "ColorAnimation" myColorAnimation (newComponentInterface myColorAnimation)
    registerComponent "SizeAnimation"  mySizeAnimation  (newComponentInterface mySizeAnimation)
    registerComponent "PositionAnimation"  myPositionAnimation  (newComponentInterface myPositionAnimation)
    registerComponent "RotationAnimation"  myRotationAnimation  (newComponentInterface myRotationAnimation)

tickAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
tickAnimationSystem = whenWorldPlaying $ do
    now <- realToFrac <$> getNow

    tickComponentAnimation now myColorAnimation setColor
    tickComponentAnimation now mySizeAnimation  setSize
    tickComponentAnimation now myPositionAnimation setPosition
    tickComponentAnimation now myRotationAnimation setRotationQ

tickComponentAnimation :: MonadState ECS m
                       => DiffTime
                       -> Key (EntityMap (Animation struct))
                       -> (struct -> ReaderT EntityID m a)
                       -> m ()
tickComponentAnimation now animComponentKey setter =
    forEntitiesWithComponent animComponentKey $
        \(entityID, animation) -> inEntity entityID $ do
            let evaled = evalAnim now animation

            _ <- setter (evanResult evaled)
            when (evanRunning evaled == False) $ do
                removeComponent animComponentKey


animateSizeTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> DiffTime -> m ()
animateSizeTo newSize time = do
    currentSize <- getSize
    animation <- makeAnimation time currentSize newSize
    mySizeAnimation ==> animation
