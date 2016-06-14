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
tickComponentAnimation now animComponentKey setterAction =
    forEntitiesWithComponent animComponentKey $
        \(entityID, animation) -> inEntity entityID $ do
            let evaled = evalAnim now animation

            _ <- setterAction (evanResult evaled)
            when (evanRunning evaled == False) $ do
                removeComponent animComponentKey

animateEntitySizeTo :: (MonadIO m, MonadState ECS m) => EntityID -> V3 GLfloat -> DiffTime -> m ()
animateEntitySizeTo entityID newSize time = inEntity entityID $ animateSizeTo newSize time

animateSizeTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> DiffTime -> m ()
animateSizeTo newSize time = do
    let time' = max 0.001 time
    currentSize <- getSize
    animation <- makeAnimation time' currentSize newSize
    mySizeAnimation ==> animation


animateSizeFromTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> V3 GLfloat -> DiffTime -> m ()
animateSizeFromTo fromSize toSize time = do
    let time' = max 0.001 time
    currentSize <- getSize
    animation <- makeAnimation time' fromSize toSize
    mySizeAnimation ==> animation


animateColor :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => DiffTime -> V4 GLfloat -> V4 GLfloat -> m ()
animateColor time fromColor toColor = do
    animation <- makeAnimation time fromColor toColor
    myColorAnimation ==> animation
