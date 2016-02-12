{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Animation where
import PreludeExtra
import Data.ECS
import Rumpus.Systems.Physics
import Rumpus.Systems.Shared

defineComponentKeyWithType "ColorAnimation" [t|Animation (V4 GLfloat)|]
defineComponentKeyWithType "SizeAnimation" [t|Animation (V3 GLfloat)|]

initAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
initAnimationSystem = do
    registerComponent "ColorAnimation" cmpColorAnimation (newComponentInterface cmpColorAnimation)
    registerComponent "SizeAnimation" cmpSizeAnimation (newComponentInterface cmpSizeAnimation)

    return ()

tickAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
tickAnimationSystem = do
    now <- getNow
    forEntitiesWithComponent cmpColorAnimation $ \(entityID, animation) -> do
        let evaled = evalAnim now animation

        setComponent cmpColor (evanResult evaled) entityID
        when (evanRunning evaled == False) $ do
            removeComponent cmpColorAnimation entityID

    forEntitiesWithComponent cmpSizeAnimation $ \(entityID, animation) -> do
        let evaled = evalAnim now animation

        setEntitySize (evanResult evaled) entityID
        when (evanRunning evaled == False) $ do
            removeComponent cmpSizeAnimation entityID
