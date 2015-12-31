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

collectControlEvents :: (MonadState World m, MonadIO m) => VRPal -> M44 GLfloat -> [Hand] -> m ()
collectControlEvents VRPal{..} headM44 hands = do

    -- Grab the old events for comparison
    lastEvents <- use wldEvents
    -- Clear the events list
    wldEvents .= []

    -- Generate ButtonDown and ButtonUp events for Hand controllers. This should go in VRPal.
    let lastHands = catMaybes $ map (\case
            (VREvent (LeftHandEvent (HandEvent hand))) -> Just hand
            (VREvent (RightHandEvent (HandEvent hand))) -> Just hand
            _ -> Nothing) lastEvents

    let handTriples = zip3 lastHands hands [LeftHandEvent, RightHandEvent]
    forM_ handTriples $ \(oldHand, newHand, handEventCons) -> 
        forM_ buttonPairs $ \(buttonEventCons, buttonView) -> do
            let maybeEvent = case (buttonView oldHand, buttonView newHand) of
                    (True, False) -> Just ButtonUp
                    (False, True) -> Just ButtonDown
                    _             -> Nothing
            forM_ maybeEvent $ \buttonDownUp -> 
                wldEvents %= (VREvent (handEventCons (buttonEventCons buttonDownUp)) :)


    wldEvents <>= map VREvent ( HeadEvent headM44 : zipWith ($) [LeftHandEvent, RightHandEvent] (map HandEvent hands) )
    -- Gather GLFW Pal events
    processEvents gpEvents $ \e -> do
        closeOnEscape gpWindow e
        wldEvents %= (GLFWEvent e:)

buttonPairs :: [(ButtonDown -> HandEvent, Hand -> Bool)]
buttonPairs = [ (ButtonGrip, view hndGrip)
              , (ButtonTrigger, view (hndTrigger . to (> 0.5)))
              , (ButtonA, view hndButtonA)
              , (ButtonB, view hndButtonB)
              , (ButtonC, view hndButtonC)
              , (ButtonD, view hndButtonD)
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
onLeftHandEvent (VREvent (LeftHandEvent handEvent)) f = f handEvent
onLeftHandEvent _ _ = return ()

onRightHandEvent :: Monad m => WorldEvent -> (HandEvent -> m ()) -> m ()
onRightHandEvent (VREvent (RightHandEvent handEvent)) f = f handEvent
onRightHandEvent _ _ = return ()