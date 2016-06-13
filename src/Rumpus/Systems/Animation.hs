module Rumpus.Systems.Animation where

import PreludeExtra
import Rumpus.Systems.Shared
--import Rumpus.Systems.PlayPause
import Rumpus.Systems.Physics
import Rumpus.Systems.Controls

initAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
initAnimationSystem = do
    registerComponent "ColorAnimation"     myColorAnimation     (newComponentInterface myColorAnimation)
    registerComponent "SizeAnimation"      mySizeAnimation      (newComponentInterface mySizeAnimation)
    registerComponent "PositionAnimation"  myPositionAnimation  (newComponentInterface myPositionAnimation)
    registerComponent "RotationAnimation"  myRotationAnimation  (newComponentInterface myRotationAnimation)

tickAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
--tickAnimationSystem = whenWorldPlaying $ do
tickAnimationSystem = do
    now <- realToFrac <$> getNow

    tickComponentAnimation now myColorAnimation setColor
    tickComponentAnimation now mySizeAnimation  setSizeNoAnim
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
            traceM $ show (now, animStart animation, animDuration animation)
            when (evanRunning evaled == False) $ do
                removeComponent animComponentKey


animateSizeTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> DiffTime -> m ()
animateSizeTo newSize time = do
    let time' = max 0.001 time
    currentSize <- getSize
    animation <- makeAnimation time' currentSize newSize
    mySizeAnimation ==> animation


animateColor :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => DiffTime -> V4 GLfloat -> V4 GLfloat -> m ()
animateColor time fromColor toColor = do
    animation <- makeAnimation time fromColor toColor
    myColorAnimation ==> animation
