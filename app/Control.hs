{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Control where
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.VR.Pal

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens.Extra
import Data.Maybe

import Types

controlEventsSystem :: (MonadState World m, MonadReader WorldStatic m, MonadIO m) => M44 GLfloat -> [Hand] -> m ()
controlEventsSystem headM44 hands = do
    VRPal{..} <- view wlsVRPal

    -- Grab the old events for comparison
    lastEvents <- use wldEvents
    -- Clear the events list
    wldEvents .= []

    -- Gather GLFW Pal events
    processEvents gpEvents $ \e -> do
        closeOnEscape gpWindow e
        onKeyDown e Key'Space $ toggleWorldPlaying

        wldEvents %= (GLFWEvent e:)

    hands' <- if gpRoomScale == RoomScale
        then return hands
        else emulateRightHand

    -- Generate ButtonDown and ButtonUp events for Hand controllers. This should go in VRPal.
    let lastHands = catMaybes $ map (\case
            (VREvent (HandEvent _ (HandStateEvent hand))) -> Just hand
            _ -> Nothing) lastEvents

    let handTriples = zip3 lastHands hands' [LeftHand, RightHand]
    forM_ handTriples $ \(oldHand, newHand, whichHand) -> 
        forM_ buttonPairs $ \(whichButton, buttonView) -> do
            let maybeEvent = case (buttonView oldHand, buttonView newHand) of
                    (True, False) -> Just ButtonUp
                    (False, True) -> Just ButtonDown
                    _             -> Nothing
            forM_ maybeEvent $ \buttonDownUp -> 
                wldEvents %= (VREvent (HandEvent whichHand (HandButtonEvent whichButton buttonDownUp)) :)
    
    wldEvents <>= map VREvent
        ( HeadEvent headM44
        : zipWith ($) [HandEvent LeftHand, HandEvent RightHand] (map HandStateEvent hands')
        )

    
emulateRightHand :: (MonadState World m, MonadReader WorldStatic m, MonadIO m) => m [Hand]
emulateRightHand = do
    VRPal{..}  <- view wlsVRPal
    player     <- use wldPlayer
    projM44    <- getWindowProjection gpWindow 45 0.1 1000
    mouseRay   <- cursorPosToWorldRay gpWindow projM44 player
    mouseState <- getMouseButton gpWindow MouseButton'1
    let handZ = 5 -- TODO: control with scroll/pinch?
        handPosition = projectRay mouseRay handZ
        trigger = if mouseState == MouseButtonState'Pressed then 1 else 0
        handMatrix = identity & translation .~ handPosition
        hand = emptyHand 
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
    return [hand]


toggleWorldPlaying :: (MonadState World m) => m ()
toggleWorldPlaying = wldPlaying %= not

buttonPairs :: [(HandButton, Hand -> Bool)]
buttonPairs = [ (HandButtonGrip, view hndGrip)
              , (HandButtonTrigger, view (hndTrigger . to (> 0.5)))
              , (HandButtonA, view hndButtonA)
              , (HandButtonB, view hndButtonB)
              , (HandButtonC, view hndButtonC)
              , (HandButtonD, view hndButtonD)
              ]

withLeftHandEvents :: MonadState World m => (HandEvent -> m ()) -> m ()
withLeftHandEvents f = do
  events <- use wldEvents
  forM_ events (\e -> onLeftHandEvent e f)

withRightHandEvents :: MonadState World m => (HandEvent -> m ()) -> m ()
withRightHandEvents f = do
  events <- use wldEvents
  forM_ events (\e -> onRightHandEvent e f)

onLeftHandEvent :: Monad m => WorldEvent -> (HandEvent -> m ()) -> m ()
onLeftHandEvent (VREvent (HandEvent LeftHand handEvent)) f = f handEvent
onLeftHandEvent _ _ = return ()

onRightHandEvent :: Monad m => WorldEvent -> (HandEvent -> m ()) -> m ()
onRightHandEvent (VREvent (HandEvent RightHand handEvent)) f = f handEvent
onRightHandEvent _ _ = return ()
