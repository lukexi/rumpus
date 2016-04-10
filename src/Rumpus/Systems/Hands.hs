{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Hands where
import Rumpus.Systems.Controls
import Rumpus.Systems.Physics
import Rumpus.Systems.Collisions
import Rumpus.Systems.Shared
import PreludeExtra
data HandsSystem = HandsSystem 
    { _hndLeftHand  :: EntityID
    , _hndRightHand :: EntityID
    --, _hndHead      :: EntityID
    } deriving Show
makeLenses ''HandsSystem

defineSystemKey ''HandsSystem

--tickHandsSystem = do

startHandsSystem :: (MonadState ECS m, MonadIO m) => m ()
startHandsSystem = do
    vrPal <- viewSystem sysControls ctsVRPal
    --when (gpRoomScale vrPal == RoomScale) $ do
    let handColor = V4 0.6 0.6 0.9 1

    leftHandID <- spawnEntity Transient $ do
        myColor             ==> handColor
        mySize              ==> V3 0.1 0.1 0.3
        myShapeType         ==> CubeShape
        myPhysicsProperties ==> [Kinematic, NoContactResponse, Static]
        myMass              ==> 0
        myOnCollisionStart  ==> \_ impulse -> do
            hapticPulse LeftHand (floor $ impulse * 10000)
    rightHandID <- spawnEntity Transient $ do
        myColor             ==> handColor
        mySize              ==> V3 0.1 0.1 0.3
        myShapeType         ==> CubeShape
        myPhysicsProperties ==> [Kinematic, NoContactResponse, Static]
        myMass              ==> 0
        myOnCollisionStart  ==> \_ impulse -> 
            hapticPulse RightHand (floor $ impulse * 10000)
    registerSystem sysHands $ HandsSystem
            { _hndLeftHand  = leftHandID
            , _hndRightHand = rightHandID
            }

    return ()

-- FIXME should update and get hndHead instead
getHeadPose :: (MonadState ECS m) => m (M44 GLfloat)
getHeadPose = viewSystem sysControls ctsHeadPose

getLeftHandID :: (MonadState ECS m) => m EntityID
getLeftHandID  = viewSystem sysHands hndLeftHand

getRightHandID :: (MonadState ECS m) => m EntityID
getRightHandID = viewSystem sysHands hndRightHand


getHandIDs :: (MonadState ECS m) => m [(WhichHand, EntityID)]
getHandIDs = do
    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    return [ (LeftHand, leftHandID)
           , (RightHand, rightHandID)
           ]