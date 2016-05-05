{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.SceneEditor where
import PreludeExtra

--import Rumpus.Systems.Controls
import Rumpus.Systems.Drag
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics

import Rumpus.Systems.Attachment
--import Rumpus.Systems.CodeEditor
import Rumpus.Systems.KeyPads
import Rumpus.Systems.Haptics
import Rumpus.Systems.Selection
--import Rumpus.Systems.Scene

data SceneEditorSystem = SceneEditorSystem
    { _sedCurrentEditorFrame :: !(Maybe EntityID)
    }
makeLenses ''SceneEditorSystem
defineSystemKey ''SceneEditorSystem


initSceneEditorSystem :: MonadState ECS m => m ()
initSceneEditorSystem = do
    registerSystem sysSceneEditor $ SceneEditorSystem Nothing

clearSelection :: (MonadIO m, MonadState ECS m) => m ()
clearSelection = do

    hideKeyPads

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

            showKeyPads
            --addEditorFrame entityID

    return ()

removeCurrentEditorFrame :: (MonadIO m, MonadState ECS m) => m ()
removeCurrentEditorFrame = traverseM_ (viewSystem sysSceneEditor sedCurrentEditorFrame) removeEntity

spawnNewEntityAtPose :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m EntityID
spawnNewEntityAtPose pose = spawnEntity $ do
    myPose  ==> pose
    myShape ==> Cube
    mySize  ==> 0.5
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
                initiateGrab whichHand handEntityID otherHandEntityID
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

initiateGrab :: WhichHand -> EntityID -> EntityID -> ECSMonad ()
initiateGrab whichHand handEntityID otherHandEntityID = do
    -- Find the entities overlapping the hand, and attach them to it
    overlappingEntityIDs <- filterStaticEntityIDs
                                =<< getEntityOverlappingEntityIDs handEntityID

    when (null overlappingEntityIDs) $ do
        clearSelection
        --didPlaceCursor <- raycastCursor handEntityID

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
                    grabEntity handEntityID duplicateID
            | hasDragFunction ->
                beginDrag handEntityID grabbedID
            | otherwise ->
                grabEntity handEntityID grabbedID

grabEntity :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> m ()
grabEntity handEntityID grabbedID = do
    selectEntity grabbedID
    attachEntity handEntityID grabbedID True
