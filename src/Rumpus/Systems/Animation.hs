{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Animation where
import PreludeExtra
import Rumpus.ECS
import Rumpus.Types
import Rumpus.Systems.Physics
import Rumpus.Systems.Shared

defineComponentKeyWithType "ColorAnimation" [t|Animation (V4 GLfloat)|]
defineComponentKeyWithType "SizeAnimation" [t|Animation (V3 GLfloat)|]

animationSystem :: (MonadIO m, MonadState World m) => m ()
animationSystem = do
    now <- getNow
    forEntitiesWithComponent colorAnimationKey $ \(entityID, animation) -> do
        let evaled = evalAnim now animation

        setComponent colorKey (evanResult evaled) entityID
        when (evanRunning evaled == False) $ do
            removeComponentFromEntity colorAnimationKey entityID

    forEntitiesWithComponent sizeAnimationKey  $ \(entityID, animation) -> do
        let evaled = evalAnim now animation

        setEntitySize (evanResult evaled) entityID
        when (evanRunning evaled == False) $ do
            removeComponentFromEntity sizeAnimationKey entityID
