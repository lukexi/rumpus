{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.Hands where
import Rumpus.Systems.Controls
import Rumpus.Systems.Physics
import Rumpus.Systems.Collisions
import Rumpus.Systems.Shared
import PreludeExtra
data HandsSystem = HandsSystem 
    { _hndLeftHand  :: EntityID
    , _hndRightHand :: EntityID
    , _hndBeams :: Map WhichHand EntityID
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
            , _hndBeams = mempty
            }

    return ()

beginBeam :: (MonadState ECS m, MonadIO m) => WhichHand -> m ()
beginBeam whichHand = do

    beamID <- spawnEntity $ do
        myShape      ==> Cube
        myProperties ==> [NoPhysicsShape]

    modifySystemState sysHands $ hndBeams . at whichHand ?= beamID
    updateBeam whichHand
            
updateBeam :: (MonadState ECS m, MonadIO m) => WhichHand -> m ()
updateBeam whichHand = traverseM_ (viewSystem sysHands (hndBeams . at whichHand)) $ \beamID -> do
    handID <- getHandID whichHand
    handPose <- getEntityPose handID

    let handRay = poseToRay (poseFromMatrix handPose) (V3 0 0 (-1)) :: Ray GLfloat

    mRayResult <- castRay handRay
    let noHitLocation = V3 0 0 (-1000) *! handPose ^. _m33
    (hitLocation, teleportable) <- fmap (fromMaybe (noHitLocation, False)) . forM mRayResult $ \RayResult{..} -> do
        entityID <- unCollisionObjectID <$> getCollisionObjectID rrCollisionObject
        teleportable <- getIsTeleportable entityID
        
        when teleportable $ do
            hapticPulse whichHand 1000

        return (rrLocation, teleportable)

    let handLocation = handPose ^. translation 
        rayLength = distance handLocation hitLocation
        rayCenter = handLocation + (hitLocation - handLocation) / 2

    -- Update ray's postition/size
    runEntity beamID $ do
        setPose (handPose & translation .~ rayCenter)
        setSize (V3 0.05 0.05 rayLength)
        setColor $ if teleportable then V4 0 1 0 1 else V4 0.2 0.2 0.2 1

endBeam :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
endBeam whichHand = traverseM_ (viewSystem sysHands (hndBeams . at whichHand)) $ \beamID -> do
    removeEntity beamID
    modifySystemState sysHands $ hndBeams . at whichHand .= Nothing

    handID <- getHandID whichHand
    handPose <- getEntityPose handID

    let handRay = poseToRay (poseFromMatrix handPose) (V3 0 0 (-1)) :: Ray GLfloat

    mRayResult <- castRay handRay
    forM_ mRayResult $ \RayResult{..} -> do
        entityID <- unCollisionObjectID <$> getCollisionObjectID rrCollisionObject
        teleportable <- getIsTeleportable entityID
        when teleportable $ do
            pose          <- getEntityPose entityID
            V3 _ height _ <- getEntitySize entityID
            let V3 x y z = pose ^. translation
            setPlayerPosition (V3 x (y+height/2) z)

-- FIXME should update and get hndHead instead
getHeadPose :: (MonadState ECS m) => m (M44 GLfloat)
getHeadPose = viewSystem sysControls ctsHeadPose

getLeftHandID :: (MonadState ECS m) => m EntityID
getLeftHandID  = viewSystem sysHands hndLeftHand

getRightHandID :: (MonadState ECS m) => m EntityID
getRightHandID = viewSystem sysHands hndRightHand

getHandID :: MonadState ECS m => WhichHand -> m EntityID
getHandID whichHand = case whichHand of
    LeftHand  -> getLeftHandID
    RightHand -> getRightHandID

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