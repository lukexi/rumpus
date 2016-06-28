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

animateEntitySizeTo :: (MonadIO m, MonadState ECS m)
                    => EntityID -> V3 GLfloat -> DiffTime -> m ()
animateEntitySizeTo entityID newSize time =
    inEntity entityID $ animateSizeTo newSize time

animateSizeTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
              => V3 GLfloat -> DiffTime -> m ()
animateSizeTo newSize time = do
    let time' = max 0.001 time
    currentSize <- getSize
    animation <- makeAnimation time' currentSize newSize
    mySizeAnimation ==> animation


animateSizeFromTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                  => V3 GLfloat -> V3 GLfloat -> DiffTime -> m ()
animateSizeFromTo fromSize toSize time = do
    let time' = max 0.001 time
    animation <- makeAnimation time' fromSize toSize
    mySizeAnimation ==> animation

animateSizeInFrom0 :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                   => DiffTime -> m ()
animateSizeInFrom0 time = do
    currentSize <- getSize
    animateSizeFromTo 0 currentSize time

animateSizeOutTo0 :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                  => DiffTime -> m ()
animateSizeOutTo0 time = animateSizeTo 0 time

animateColor :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
             => V4 GLfloat -> V4 GLfloat -> DiffTime -> m ()
animateColor fromColor toColor time = do
    animation <- makeAnimation time fromColor toColor
    myColorAnimation ==> animation

animateColorTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
               => V4 GLfloat -> DiffTime -> m ()
animateColorTo toColor time = do
    currentColor <- getColor
    animateColor currentColor toColor time

animateEntityColorTo :: (MonadIO m, MonadState ECS m)
                     => EntityID -> V4 GLfloat -> DiffTime -> m ()
animateEntityColorTo entityID toColor time = inEntity entityID $
    animateColorTo toColor time

animatePositionTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                  => V3 GLfloat -> DiffTime -> m ()
animatePositionTo toPosition time = do
    currentPosition <- getPosition
    animation <- makeAnimation time currentPosition toPosition
    myPositionAnimation ==> animation


animateRotationTo :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                  => Quaternion GLfloat -> DiffTime -> m ()
animateRotationTo toRotation time = do
    currentRotation <- getRotation
    animation <- makeAnimation time currentRotation toRotation
    myRotationAnimation ==> animation
