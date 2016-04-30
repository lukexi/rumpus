{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

module Rumpus.Systems.Controls where
import PreludeExtra
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Selection

import Foreign.C.Types
import qualified Graphics.VR.Pal as VRPal

data ControlsSystem = ControlsSystem
    { _ctsVRPal          :: !VRPal
    , _ctsPlayer         :: !(M44 GLfloat)
    , _ctsEvents         :: ![VRPalEvent]
    , _ctsHeadPose       :: !(M44 GLfloat) -- FIXME this should just update the entity representing the head in Hands
    , _ctsInternalEvents :: !(TChan VRPalEvent)
    }
makeLenses ''ControlsSystem
defineSystemKey ''ControlsSystem

getNow :: (MonadState ECS m, MonadIO m) => m GLfloat
getNow = do
    vrPal <- viewSystem sysControls ctsVRPal
    realToFrac . utctDayTime <$> VRPal.getNow vrPal 

hapticPulse :: (MonadIO m, MonadState ECS m) => WhichHand -> CUShort -> m ()
hapticPulse whichHand duration = do
    let axis = 0 -- none of the others seem to work??
    vrPal <- viewSystem sysControls ctsVRPal
    triggerHandHapticPulse vrPal whichHand axis duration

initControlsSystem :: (MonadState ECS m, MonadIO m) => VRPal -> m ()
initControlsSystem vrPal = do
    internalEvents <- liftIO newTChanIO
    registerSystem sysControls $ ControlsSystem
        { _ctsVRPal = vrPal
        , _ctsPlayer = if gpRoomScale vrPal == RoomScale
                        then identity
                        else mkTransformation (axisAngle (V3 0 1 0) pi) (V3 0 1 0) 
        , _ctsEvents = []
        , _ctsHeadPose = identity
        , _ctsInternalEvents = internalEvents
        }


sendInternalEvent :: (MonadIO m, MonadState ECS m) => VRPalEvent -> m ()
sendInternalEvent event = do
    internalEvents <- viewSystem sysControls ctsInternalEvents
    liftIO . atomically $ writeTChan internalEvents event

tickControlEventsSystem :: (MonadState ECS m, MonadIO m) => M44 GLfloat -> [VRPalEvent] -> m ()
tickControlEventsSystem headM44 events = modifySystemState sysControls $ do
    ctsHeadPose .= headM44

    VRPal{..} <- use ctsVRPal
    
    -- Clear the events list
    ctsEvents .= events

    -- Gather internal events
    internalEvents <- liftIO . atomically . exhaustTChan =<< use ctsInternalEvents
    ctsEvents <>= internalEvents

    when (gpRoomScale /= RoomScale) $ do
        hasSelection <- isJust <$> lift getSelectedEntityID
        unless hasSelection $ 
            applyWASD gpWindow (ctsPlayer . iso poseFromMatrix transformationFromPose)

    use ctsEvents >>= mapM_ (\case
        VREvent (HandEvent _ (HandButtonEvent HandButtonStart ButtonDown)) -> lift toggleWorldPlaying
        GLFWEvent e -> closeOnEscape gpWindow e
        --GLFWEvent e -> onKeyDown e Key'Space (lift toggleWorldPlaying)
        _ -> return ())

setPlayerPosition :: MonadState ECS m => V3 GLfloat -> m ()
setPlayerPosition position = modifySystemState sysControls $
    ctsPlayer .= mkTransformation (axisAngle (V3 0 1 0) 0) position


raycastCursorHits :: (MonadIO m, MonadState ECS m)
                  => Window -> DynamicsWorld -> M44 GLfloat -> m ()
raycastCursorHits window dynamicsWorld projMat = withSystem_ sysControls $ \controlSystem -> do
    let playerPose = controlSystem ^. ctsPlayer
    cursorRay <- cursorPosToWorldRay window projMat (poseFromMatrix playerPose)

    mRayResult <- rayTestClosest dynamicsWorld cursorRay
    forM_ mRayResult $ \rayResult -> do
        bodyID <- getCollisionObjectID (rrCollisionObject rayResult)
        -- Convert the hit location into model space
        -- (position, orientation) <- getBodyState (cube ^. cubBody)
        -- let model = mkTransformation orientation position
        --     pointOnModel = worldPointToModelPoint model (rrLocation rayResult)
        let _hitInWorld = rrLocation rayResult
            entityID = fromIntegral (unCollisionObjectID bodyID) :: EntityID
        return entityID
