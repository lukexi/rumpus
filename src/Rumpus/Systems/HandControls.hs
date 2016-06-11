module Rumpus.Systems.HandControls where
import PreludeExtra

import Rumpus.Systems.Drag
import Rumpus.Systems.Hands
import Rumpus.Systems.Physics
import Rumpus.Systems.Attachment
import Rumpus.Systems.Creator
import Rumpus.Systems.Haptics
import Rumpus.Systems.Teleport
import Rumpus.Systems.SceneWatcher
import Rumpus.Systems.CodeProtect
import Rumpus.Systems.Selection
import Rumpus.Systems.KeyPads
import Rumpus.Systems.Shared


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
            HandButtonEvent HandButtonGrip ButtonDown ->
                beginBeam whichHand
            HandButtonEvent HandButtonGrip ButtonUp ->
                endBeam whichHand
            HandButtonEvent HandButtonTrigger ButtonDown ->
                initiateGrab whichHand handEntityID otherHandEntityID
            HandButtonEvent HandButtonTrigger ButtonUp -> do
                maybeHeldEntity <- getOneEntityAttachment handEntityID
                wasDestroyed    <- checkForDestruction whichHand

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
            HandButtonEvent HandButtonStart ButtonDown ->
                openEntityLibrary whichHand
            HandButtonEvent HandButtonStart ButtonUp ->
                closeEntityLibrary whichHand
            _ -> return ()

    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    withLeftHandEvents  (editSceneWithHand LeftHand leftHandID  rightHandID)
    withRightHandEvents (editSceneWithHand RightHand rightHandID leftHandID)


filterUngrabbableEntityIDs :: MonadState ECS m => [EntityID] -> m [EntityID]
filterUngrabbableEntityIDs = filterM (fmap (notElem Ungrabbable) . getEntityBodyFlags)

getGrabbableEntityIDs :: EntityID -> ECSMonad [EntityID]
getGrabbableEntityIDs = filterUngrabbableEntityIDs <=< getEntityOverlappingEntityIDs

initiateGrab :: WhichHand -> EntityID -> EntityID -> ECSMonad ()
initiateGrab whichHand handEntityID _otherHandEntityID = do
    -- Find the entities overlapping the hand, and attach them to it
    overlappingEntityIDs <- getGrabbableEntityIDs handEntityID

    when (null overlappingEntityIDs)
        clearSelection
        --didPlaceCursor <- raycastCursor handEntityID

    forM_ (listToMaybe overlappingEntityIDs) $ \grabbedID -> do
        handPose <- getEntityPose handEntityID
        beginHapticDrag whichHand handPose

        wantsToHandleDrag <- getEntityDragOverride grabbedID
        unless wantsToHandleDrag $
            grabEntity handEntityID grabbedID

        -- Call beginDrag after grabEntity so we can override selection if we want (i.e., call clearSelection)
        beginDrag handEntityID grabbedID

grabEntity :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> m ()
grabEntity handEntityID grabbedID = do
    selectEntity grabbedID
    attachEntityToEntity handEntityID grabbedID True

--grabDuplicateEntity grabbedID otherHandEntityID = do
    --isBeingHeldByOtherHand <- isEntityAttachedTo grabbedID otherHandEntityID
    --when isBeingHeldByOtherHand $ do
    --    -- Trying things out with this disabled, as it's too
    --    -- easy to cause performance problems by effortlessly
    --    -- duplicating expensive objects. Effort to dupe should
    --    -- roughly scale with how often we want users to do it.
    --    let allowDuplication = False
    --    when allowDuplication $ do
    --        duplicateID <- duplicateEntity Persistent grabbedID
    --        --forkCode grabbedID duplicateID
    --        grabEntity handEntityID duplicateID
    --return isBeingHeldByOtherHand
