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
import Rumpus.Systems.SceneWatcher
import Rumpus.Systems.CodeProtect
import Rumpus.Systems.Selection
import Rumpus.Systems.KeyPads

tickHandControlsSystem :: ECSMonad ()
tickHandControlsSystem = runUserScriptsWithTimeout_ $ do
    let editSceneWithHand whichHand handEntityID otherHandEntityID event = case event of
            HandStateEvent hand -> do
                -- Shift the hands down a bit, since OpenVR gives us the position
                -- of center of the controller's ring rather than its body
                let newHandPoseRaw = hand ^. hndMatrix
                    handRotation = newHandPoseRaw ^. _m33
                    handOffset = handRotation !* V3 0 0 0.05
                    newHandPose = newHandPoseRaw & translation +~ handOffset
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
                maybeHeldEntity <- getOneEntityAttachment handEntityID
                wasDestroyed <- checkForDestruction whichHand

                endHapticDrag whichHand
                endDrag handEntityID
                detachAttachedEntities handEntityID

                forM_ maybeHeldEntity $ \entityID -> do
                    selectedEntityID <- getSelectedEntityID
                    if Just entityID == selectedEntityID && wasDestroyed
                        then do
                            clearSelectedEntityID
                            hideKeyPads
                        else do
                            isPersistent <- isEntityPersistent entityID
                            when isPersistent $
                                sceneWatcherSaveEntity entityID
            HandButtonEvent HandButtonStart ButtonDown -> do
                openEntityLibrary whichHand
            HandButtonEvent HandButtonStart ButtonUp -> do
                closeEntityLibrary whichHand
            _ -> return ()

    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    withLeftHandEvents  (editSceneWithHand LeftHand leftHandID  rightHandID)
    withRightHandEvents (editSceneWithHand RightHand rightHandID leftHandID)
