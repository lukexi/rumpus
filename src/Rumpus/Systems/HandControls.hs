{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.HandControls where
import PreludeExtra

import Rumpus.Systems.Drag
import Rumpus.Systems.Hands
import Rumpus.Systems.Physics
import Rumpus.Systems.Attachment
import Rumpus.Systems.Creator
import Rumpus.Systems.Haptics
import Rumpus.Systems.Teleport
import Rumpus.Systems.SceneEditor

tickHandControlsSystem :: ECSMonad ()
tickHandControlsSystem = do
    let editSceneWithHand whichHand handEntityID otherHandEntityID event = case event of
            HandStateEvent hand -> do
                let newHandPose = hand ^. hndMatrix
                setEntityPose handEntityID newHandPose
                continueDrag handEntityID
                continueHapticDrag whichHand newHandPose
                updateBeam whichHand
            HandButtonEvent HandButtonGrip ButtonDown -> do
                beginBeam whichHand
            HandButtonEvent HandButtonGrip ButtonUp -> do
                endBeam whichHand
            HandButtonEvent HandButtonTrigger ButtonDown -> do
                initiateGrab whichHand handEntityID otherHandEntityID
            HandButtonEvent HandButtonTrigger ButtonUp -> do
                endHapticDrag whichHand
                endDrag handEntityID
                detachAttachedEntities handEntityID

                -- Saving is currently disabled to simplify the alpha release
                -- (code will still be saved automatically)
                --saveScene
            HandButtonEvent HandButtonStart ButtonDown -> do
                primeNewEntity whichHand
            HandButtonEvent HandButtonStart ButtonUp -> do
                unprimeNewEntity whichHand
            _ -> return ()

    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    withLeftHandEvents  (editSceneWithHand LeftHand leftHandID  rightHandID)
    withRightHandEvents (editSceneWithHand RightHand rightHandID leftHandID)
