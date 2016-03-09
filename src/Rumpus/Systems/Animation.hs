{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Animation where
import PreludeExtra hiding (Key)
import Rumpus.Systems.Shared
import Rumpus.Systems.PlayPause
import Data.ECS.Vault

defineComponentKeyWithType "ColorAnimation" [t|Animation (V4 GLfloat)|]
defineComponentKeyWithType "SizeAnimation"  [t|Animation (V3 GLfloat)|]
-- defineComponentKeyWithType "PoseAnimation"  [t|Animation (Pose GLfloat)|]

initAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
initAnimationSystem = do
    registerComponent "ColorAnimation" cmpColorAnimation (newComponentInterface cmpColorAnimation)
    registerComponent "SizeAnimation"  cmpSizeAnimation  (newComponentInterface cmpSizeAnimation)
    -- registerComponent "PoseAnimation"  cmpPoseAnimation  (newComponentInterface cmpPoseAnimation)

tickAnimationSystem :: (MonadIO m, MonadState ECS m) => m ()
tickAnimationSystem = whenWorldPlaying $ do
    now <- getNow
    
    tickComponentAnimation now cmpColorAnimation cmpColor
    tickComponentAnimation now cmpSizeAnimation  cmpSize
    -- tickComponentAnimation now cmpPoseAnimation  cmpPose

tickComponentAnimation :: MonadState ECS m 
                       => DiffTime
                       -> Key (EntityMap (Animation struct))
                       -> Key (EntityMap struct)
                       -> m ()
tickComponentAnimation now animComponentKey componentKey = 
    forEntitiesWithComponent animComponentKey $ 
        \(entityID, animation) -> runEntity entityID $ do
            let evaled = evalAnim now animation

            componentKey ==> evanResult evaled
            when (evanRunning evaled == False) $ do
                removeComponent animComponentKey
