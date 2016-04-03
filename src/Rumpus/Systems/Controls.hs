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
import qualified Graphics.VR.Pal as VRPal
import Graphics.VR.Pal (VRPalEvent)
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

initControlsSystem :: (MonadState ECS m, MonadIO m) => VRPal -> m ()
initControlsSystem vrPal = do
    internalEvents <- liftIO newTChanIO
    registerSystem sysControls $ ControlsSystem
        { _ctsVRPal = vrPal
        , _ctsPlayer = if gpRoomScale vrPal == RoomScale
                        then identity
                        else mkTransformation (axisAngle (V3 0 1 0) (pi)) (V3 0 1 (-1)) 
        , _ctsEvents = []
        , _ctsHeadPose = identity
        , _ctsInternalEvents = internalEvents
        }


sendInternalEvent :: (MonadIO m, MonadState ECS m) => VRPalEvent -> m ()
sendInternalEvent event = do
    internalEvents <- viewSystem sysControls ctsInternalEvents
    liftIO . atomically $ writeTChan internalEvents event

tickControlEventsSystem :: (MonadState ECS m, MonadIO m) => M44 GLfloat -> [VREvent] -> m ()
tickControlEventsSystem headM44 vrEvents = modifySystemState sysControls $ do
    ctsHeadPose .= headM44

    vrPal@VRPal{..} <- use ctsVRPal
    
    -- Clear the events list
    ctsEvents .= map VREvent vrEvents

    -- Gather internal events
    internalEvents <- liftIO . atomically . exhaustTChan =<< use ctsInternalEvents
    ctsEvents <>= internalEvents

    -- Gather GLFW Pal events
    processEvents gpEvents $ \e -> do
        closeOnEscape gpWindow e
        ctsEvents %= (GLFWEvent e:)

    --hands' <- if gpRoomScale == RoomScale
    --    then return hands
    --    else do
    --        hasSelection <- isJust <$> lift getSelectedEntityID
    --        unless hasSelection $ 
    --            applyWASD gpWindow ctsPlayer
            
    --        player <- use ctsPlayer
    --        events <- use ctsEvents
    --        emulateRightHandVR vrPal player events

    use ctsEvents >>= mapM_ (\case
        VREvent (HandEvent _ (HandButtonEvent HandButtonStart ButtonDown)) -> lift toggleWorldPlaying
        --GLFWEvent e -> onKeyDown e Key'Space (lift toggleWorldPlaying)
        _ -> return ())

setPlayerPosition position = modifySystemState sysControls (ctsPlayer .= mkTransformation (axisAngle (V3 0 1 0) 0) position)

emulateRightHand :: (MonadIO m) => VRPal -> Pose Float -> [VRPalEvent] -> m [Hand]
emulateRightHand VRPal{..} player events = do

    projM44     <- getWindowProjection gpWindow 45 0.1 1000
    mouseRay    <- cursorPosToWorldRay gpWindow projM44 player
    mouseState1 <- getMouseButton gpWindow MouseButton'1
    mouseState2 <- getMouseButton gpWindow MouseButton'2

    forM_ events $ \case
        GLFWEvent e -> onScroll e $ \_x y ->
            liftIO $ modifyIORef' gpEmulatedHandDepthRef (+ (y*0.1))
        _ -> return ()
    handZ <- liftIO (readIORef gpEmulatedHandDepthRef)
    -- a <- getNow -- swap with line below to rotate hand for testing
    let a = 0
    let handPosition = projectRay mouseRay handZ
        trigger      = if mouseState1 == MouseButtonState'Pressed then 1 else 0
        grip         = mouseState2 == MouseButtonState'Pressed
        handMatrix   = transformationFromPose $ newPose
                                              & posPosition .~ handPosition
                                              & posOrientation .~ axisAngle (V3 0 1 0) a
        rightHand = emptyHand
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
                & hndGrip    .~ grip
    return [emptyHand, rightHand]

emulateRightHandVR :: (MonadIO m) => VRPal -> Pose Float -> [VRPalEvent] -> m [Hand]
emulateRightHandVR VRPal{..} _player events = do
    mouseState1 <- getMouseButton gpWindow MouseButton'1
    mouseState2 <- getMouseButton gpWindow MouseButton'2

    forM_ events $ \case
        GLFWEvent e -> onScroll e $ \_x y ->
            liftIO $ modifyIORef' gpEmulatedHandDepthRef (+ (y*0.1))
        _ -> return ()
    z <- liftIO (readIORef gpEmulatedHandDepthRef)

    (fromIntegral -> w, fromIntegral -> h) <- getWindowSize gpWindow
    (x, y) <- getCursorPos gpWindow
    -- a <- getNow -- swap with line below to rotate hand for testing
    let a = 0
        trigger      = if mouseState1 == MouseButtonState'Pressed then 1 else 0
        grip         = mouseState2 == MouseButtonState'Pressed
        handPosition = V3 (-x/w * 0.1) z (-y/h * 0.1)
        handMatrix   = transformationFromPose $ newPose
                                              & posPosition .~ handPosition
                                              & posOrientation .~ axisAngle (V3 0 1 0) a
        rightHand = emptyHand
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
                & hndGrip    .~ grip
    return [emptyHand, rightHand]

withLeftHandEvents :: MonadState ECS m => (HandEvent -> m ()) -> m ()
withLeftHandEvents f = withSystem_ sysControls $ \controlSystem -> do
  let events = controlSystem ^. ctsEvents
  forM_ events (\e -> onLeftHandEvent e f)

withRightHandEvents :: MonadState ECS m => (HandEvent -> m ()) -> m ()
withRightHandEvents f = withSystem_ sysControls $ \controlSystem -> do
  let events = controlSystem ^. ctsEvents
  forM_ events (\e -> onRightHandEvent e f)

onLeftHandEvent :: Monad m => VRPalEvent -> (HandEvent -> m ()) -> m ()
onLeftHandEvent (VREvent (HandEvent LeftHand handEvent)) f = f handEvent
onLeftHandEvent _ _ = return ()

onRightHandEvent :: Monad m => VRPalEvent -> (HandEvent -> m ()) -> m ()
onRightHandEvent (VREvent (HandEvent RightHand handEvent)) f = f handEvent
onRightHandEvent _ _ = return ()

onHandEvent :: Monad m => WhichHand -> VRPalEvent -> (HandEvent -> m ()) -> m ()
onHandEvent desiredHand (VREvent (HandEvent eventHand handEvent)) f 
    | desiredHand == eventHand = f handEvent
onHandEvent _ _ _ = return ()

withHandEvents :: MonadState ECS m => WhichHand -> (HandEvent -> m ()) -> m ()
withHandEvents hand f = withSystem_ sysControls $ \controlSystem -> do
  let events = controlSystem ^. ctsEvents
  forM_ events (\e -> onHandEvent hand e f)

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
