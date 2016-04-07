{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Haptics where

import Rumpus.Systems.Controls
import Rumpus.Systems.Physics
import Rumpus.Systems.Shared
import PreludeExtra

data HapticsSystem = HapticsSystem 
    { _hptLastHandTick :: Map WhichHand (V3 GLfloat)
    } deriving Show
makeLenses ''HapticsSystem

defineSystemKey ''HapticsSystem


initHapticsSystem :: MonadState ECS m => m ()
initHapticsSystem = do
    registerSystem sysHaptics $ HapticsSystem
        { _hptLastHandTick = mempty
        }

beginHapticDrag :: (MonadIO m, MonadState ECS m) => WhichHand -> M44 GLfloat -> m ()
beginHapticDrag whichHand pose = pulseWithPose whichHand (pose ^. translation) 

continueHapticDrag :: (MonadIO m, MonadState ECS m) => WhichHand -> M44 GLfloat -> m ()
continueHapticDrag whichHand pose = do
    let newPosition = pose ^. translation
    mLastPosition <- viewSystem sysHaptics (hptLastHandTick . at whichHand)
    case mLastPosition of
        Just lastPosition
            | distance lastPosition newPosition > 0.05 ->
                pulseWithPose whichHand newPosition
        _ -> return ()

pulseWithPose :: (MonadIO m, MonadState ECS m) => WhichHand -> V3 GLfloat -> m ()
pulseWithPose whichHand position = do
    modifySystemState sysHaptics $ hptLastHandTick . at whichHand ?= position
    hapticPulse whichHand 1500

endHapticDrag :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
endHapticDrag whichHand = do
    modifySystemState sysHaptics $ hptLastHandTick . at whichHand .= Nothing