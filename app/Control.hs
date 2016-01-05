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
import Control.Lens.Extra
import Data.Maybe

import Types

controlEventsSystem :: (MonadState World m, MonadIO m) => VRPal -> M44 GLfloat -> [Hand] -> m ()
controlEventsSystem VRPal{..} headM44 hands = do

    -- Grab the old events for comparison
    lastEvents <- use wldEvents
    -- Clear the events list
    wldEvents .= []

    -- Generate ButtonDown and ButtonUp events for Hand controllers. This should go in VRPal.
    let lastHands = catMaybes $ map (\case
            (VREvent (HandEvent _ (HandStateEvent hand))) -> Just hand
            _ -> Nothing) lastEvents

    let handTriples = zip3 lastHands hands [LeftHand, RightHand]
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
        : zipWith ($) [HandEvent LeftHand, HandEvent RightHand] (map HandStateEvent hands)
        )
    -- Gather GLFW Pal events
    processEvents gpEvents $ \e -> do
        closeOnEscape gpWindow e
        wldEvents %= (GLFWEvent e:)

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
