module Rumpus.Systems.Teleport where
import Rumpus.Systems.Hands
import Rumpus.Systems.Controls
import Rumpus.Systems.Physics
import Rumpus.Systems.Shared
import PreludeExtra

beginBeam :: (MonadState ECS m, MonadIO m) => WhichHand -> m ()
beginBeam whichHand = do

    beamID <- spawnEntity $ do
        myShape ==> Cube

    modifySystemState sysHands $ hndBeams . at whichHand ?= beamID
    updateBeam whichHand

updateBeam :: (MonadState ECS m, MonadIO m) => WhichHand -> m ()
updateBeam whichHand = traverseM_ (viewSystem sysHands (hndBeams . at whichHand)) $ \beamID -> do
    handID <- getHandID whichHand
    handPose <- getEntityPose handID

    let handRay = poseToRay (poseFromMatrix handPose) (V3 0 0 (-1)) :: Ray GLfloat

    mRayResult <- castRay handRay
    let noHitLocation = projectRay handRay 500
    maybeHit <- forM mRayResult $ \RayResult{..} -> do
        entityID <- unCollisionObjectID <$> getCollisionObjectID rrCollisionObject
        teleportable <- getIsTeleportable entityID

        when teleportable $ do
            hapticPulse whichHand 1000

        return (rrLocation, teleportable)

    let (hitLocation, teleportable) = fromMaybe (noHitLocation, False) maybeHit
        handLocation = handPose ^. translation
        rayLength = distance handLocation hitLocation
        rayCenter = handLocation + (hitLocation - handLocation) / 2

    -- Update ray's position/size
    inEntity beamID $ do
        setPose (handPose & translation .~ rayCenter)
        setSize (V3 0.05 0.05 rayLength)
        setColor $ if teleportable then V4 0 1 0 1 else V4 0.8 0.1 0.2 1

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
        when teleportable $
            teleportPlayerTo entityID

teleportPlayerTo :: MonadState ECS m => EntityID -> m ()
teleportPlayerTo entityID = do
    pose          <- getEntityPose entityID
    V3 _ height _ <- getEntitySize entityID
    let V3 x y z = pose ^. translation
    setPlayerPosition (V3 x (y + height / 2) z)
    scale <- getEntityTeleportScale entityID
    setPlayerScale (realToFrac scale)
