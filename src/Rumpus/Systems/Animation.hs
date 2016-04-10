{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Animation where
import PreludeExtra hiding (Key)
import Rumpus.Systems.Shared
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Controls
import Data.ECS.Vault

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
    
    tickComponentAnimation now myColorAnimation myColor
    tickComponentAnimation now mySizeAnimation  mySize
    -- tickComponentAnimation now myPoseAnimation  myPose

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
