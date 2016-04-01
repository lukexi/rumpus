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


initHapticsSystem :: MonadState ECS m => VRPal -> m ()
initHapticsSystem vrPal = do
    registerSystem sysHaptics $ HapticsSystem
            { _hptLastHandTick = mempty
            }

beginHapticDrag = tickWithPose

continueHapticDrag whichHand pose = do
    mLastPose <- viewSystem sysHaptics (hptLastHandTick . at whichHand)
    case mLastPose of
        Just lastPose
            | lastPose - pose > 10 -> 
                tickWithPose whichHand pose
        _ -> return ()

tickWithPose whichHand pose = do
    modifySystem sysHaptics $ hptLastHandTick . at whichHand ?= pose
    triggerHapticPulse whichHand 0.1 10000

endHapticDrag whichHand = do
    modifySystem sysHaptics $ hptLastHandTick . at whichHand ?= pose