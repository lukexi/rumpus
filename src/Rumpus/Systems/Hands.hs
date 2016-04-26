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

startHandsSystem :: (MonadState ECS m, MonadIO m) => m ()
startHandsSystem = do
    let handColor = V4 0.6 0.6 0.9 1

    leftHandID <- spawnEntity $ do
        myColor             ==> handColor
        mySize              ==> V3 0.1 0.1 0.3
        myShape         ==> Cube
        myProperties ==> [Floating, Ghostly, Static]
        myMass              ==> 0
        myCollisionStart  ==> \_ impulse -> do
            hapticPulse LeftHand (floor $ impulse * 10000)
    rightHandID <- spawnEntity $ do
        myColor             ==> handColor
        mySize              ==> V3 0.1 0.1 0.3
        myShape         ==> Cube
        myProperties ==> [Floating, Ghostly, Static]
        myMass              ==> 0
        myCollisionStart  ==> \_ impulse -> 
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

withLeftHandEvents :: MonadState ECS m => (HandEvent -> m ()) -> m ()
withLeftHandEvents f = withSystem_ sysControls $ \controlSystem -> do
    let events = controlSystem ^. ctsEvents
    forM_ events (\e -> onLeftHandEvent e f)

withRightHandEvents :: MonadState ECS m => (HandEvent -> m ()) -> m ()
withRightHandEvents f = withSystem_ sysControls $ \controlSystem -> do
    let events = controlSystem ^. ctsEvents
    forM_ events (\e -> onRightHandEvent e f)

withHandEvents :: MonadState ECS m => WhichHand -> (HandEvent -> m ()) -> m ()
withHandEvents hand f = withSystem_ sysControls $ \controlSystem -> do
    let events = controlSystem ^. ctsEvents
    forM_ events (\e -> onHandEvent hand e f)