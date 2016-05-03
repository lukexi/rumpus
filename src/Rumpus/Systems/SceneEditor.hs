{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.SceneEditor where
import PreludeExtra

import Rumpus.Types
--import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics

import Rumpus.Systems.Attachment
--import Rumpus.Systems.CodeEditor
import Rumpus.Systems.KeyboardHands
import Rumpus.Systems.Haptics
import Rumpus.Systems.Selection
--import Rumpus.Systems.Scene

data SceneEditorSystem = SceneEditorSystem
    { _sedCurrentEditorFrame :: !(Maybe EntityID)
    }
makeLenses ''SceneEditorSystem
defineSystemKey ''SceneEditorSystem

-- data DragFrom = DragFrom HandEntityID (M44 GLfloat)
data DragFrom = DragFrom HandEntityID (V3 GLfloat)

type Drag = V3 GLfloat -> EntityMonad ()

defineComponentKey ''DragFrom
defineComponentKey ''Drag

initSceneEditorSystem :: MonadState ECS m => m ()
initSceneEditorSystem = do
    registerSystem sysSceneEditor $ SceneEditorSystem Nothing

    registerComponent "DragFrom" myDragFrom (newComponentInterface myDragFrom)
    registerComponent "Drag"     myDrag     (newComponentInterface myDrag)

clearSelection :: (MonadIO m, MonadState ECS m) => m ()
clearSelection = do

    hideKeyboardHands

    removeCurrentEditorFrame

    clearSelectedEntityID


selectEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
selectEntity entityID = do

    getSelectedEntityID >>= \case
        Just prevSelectedID
            | prevSelectedID == entityID -> return ()
        _ -> do
            clearSelection

            setSelectedEntityID entityID

            showKeyboardHands entityID
            --addEditorFrame entityID

    return ()

removeCurrentEditorFrame :: (MonadIO m, MonadState ECS m) => m ()
removeCurrentEditorFrame = traverseM_ (viewSystem sysSceneEditor sedCurrentEditorFrame) removeEntity



beginDrag :: (MonadState ECS m, MonadIO m) => EntityID -> EntityID -> m ()
beginDrag handEntityID draggedID = do
    startPos <- view translation <$> getEntityPose handEntityID
    setEntityComponent myDragFrom (DragFrom handEntityID startPos) draggedID

continueDrag :: HandEntityID -> ECSMonad ()
continueDrag draggingHandEntityID = do
    forEntitiesWithComponent myDragFrom $ \(entityID, DragFrom handEntityID startPos) ->
        when (handEntityID == draggingHandEntityID) $ do
            currentPose <- view translation <$> getEntityPose handEntityID
            let dragDistance = currentPose - startPos

            runEntity entityID $
                withComponent_ myDrag $ \onDrag ->
                    onDrag dragDistance

endDrag :: MonadState ECS m => HandEntityID -> m ()
endDrag endingDragHandEntityID = do
    forEntitiesWithComponent myDragFrom $ \(entityID, DragFrom handEntityID _) -> do
        when (handEntityID == endingDragHandEntityID) $ do

            runEntity entityID $ removeComponent myDragFrom

spawnNewEntityAtPose :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m EntityID
spawnNewEntityAtPose pose = spawnEntity $ do
    myPose          ==> pose
    myShape     ==> Cube
    mySize          ==> 0.5
    -- myUpdateExpr  ==> ("scenes/minimal/DefaultUpdate.hs", "update")

tickSceneEditorSystem :: ECSMonad ()
tickSceneEditorSystem = do
    let editSceneWithHand whichHand handEntityID otherHandEntityID event = case event of
            HandStateEvent hand -> do
                let newHandPose = hand ^. hndMatrix
                setEntityPose newHandPose handEntityID
                continueDrag handEntityID
                continueHapticDrag whichHand newHandPose
                updateBeam whichHand
            HandButtonEvent HandButtonGrip ButtonDown -> do

                beginBeam whichHand

            HandButtonEvent HandButtonGrip ButtonUp -> do

                endBeam whichHand

            HandButtonEvent HandButtonTrigger ButtonDown -> do

                --didPlaceCursor <- raycastCursor handEntityID
                let didPlaceCursor = False
                when (not didPlaceCursor) $ do
                    -- Find the entities overlapping the hand, and attach them to it
                    overlappingEntityIDs <- filterStaticEntityIDs
                                                =<< getEntityOverlappingEntityIDs handEntityID

                    when (null overlappingEntityIDs) clearSelection

                    forM_ (listToMaybe overlappingEntityIDs) $ \grabbedID -> do
                        handPose <- getEntityPose handEntityID
                        beginHapticDrag whichHand handPose

                        hasDragFunction        <- entityHasComponent grabbedID myDrag
                        isBeingHeldByOtherHand <- isEntityAttachedTo grabbedID otherHandEntityID
                        if
                            | isBeingHeldByOtherHand -> do

                                -- Trying things out with this disabled, as it's too
                                -- easy to cause performance problems by effortlessly
                                -- duplicating expensive objects. Effort to dupe should
                                -- roughly scale with how often we want users to do it.
                                let allowDuplication = False
                                when allowDuplication $ do
                                    duplicateID <- duplicateEntity Persistent grabbedID
                                    --forkCode grabbedID duplicateID
                                    selectEntity duplicateID
                                    attachEntity handEntityID duplicateID True
                            | hasDragFunction ->
                                beginDrag handEntityID grabbedID
                            | otherwise -> do
                                selectEntity grabbedID
                                attachEntity handEntityID grabbedID True
            HandButtonEvent HandButtonTrigger ButtonUp -> do
                endHapticDrag whichHand
                endDrag handEntityID
                detachAttachedEntities handEntityID

                -- Saving is currently disabled to simplify the alpha release
                -- (code will still be saved automatically)
                --saveScene

            _ -> return ()

    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    withLeftHandEvents  (editSceneWithHand LeftHand leftHandID  rightHandID)
    withRightHandEvents (editSceneWithHand RightHand rightHandID leftHandID)


filterStaticEntityIDs :: MonadState ECS m => [EntityID] -> m [EntityID]
filterStaticEntityIDs = filterM (fmap (not . elem Static) . getEntityProperties)

