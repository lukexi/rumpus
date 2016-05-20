module Rumpus.Systems.SceneEditor where
import PreludeExtra

--import Rumpus.Systems.Controls
import Rumpus.Systems.Drag
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics

import Rumpus.Systems.Attachment
--import Rumpus.Systems.CodeEditor
import Rumpus.Systems.KeyPads
import Rumpus.Systems.Haptics
import Rumpus.Systems.Selection
--import Rumpus.Systems.EditorFrame
--import Rumpus.Systems.Scene

clearSelection :: (MonadIO m, MonadState ECS m) => m ()
clearSelection = do

    hideKeyPads

    --removeCurrentEditorFrame

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

filterUngrabbableEntityIDs :: MonadState ECS m => [EntityID] -> m [EntityID]
filterUngrabbableEntityIDs = filterM (fmap (not . elem Ungrabbable) . getEntityProperties)

initiateGrab :: WhichHand -> EntityID -> EntityID -> ECSMonad ()
initiateGrab whichHand handEntityID _otherHandEntityID = do
    -- Find the entities overlapping the hand, and attach them to it
    overlappingEntityIDs <- filterUngrabbableEntityIDs
                                =<< getEntityOverlappingEntityIDs handEntityID

    when (null overlappingEntityIDs) $ do
        clearSelection
        --didPlaceCursor <- raycastCursor handEntityID

    forM_ (listToMaybe overlappingEntityIDs) $ \grabbedID -> do
        handPose <- getEntityPose handEntityID
        beginHapticDrag whichHand handPose

        wantsToHandleDrag <- getEntityDragOverride grabbedID
        beginDrag handEntityID grabbedID
        --isBeingHeldByOtherHand <- isEntityAttachedTo grabbedID otherHandEntityID
        if
            -- | isBeingHeldByOtherHand -> do

            --    -- Trying things out with this disabled, as it's too
            --    -- easy to cause performance problems by effortlessly
            --    -- duplicating expensive objects. Effort to dupe should
            --    -- roughly scale with how often we want users to do it.
            --    let allowDuplication = False
            --    when allowDuplication $ do
            --        duplicateID <- duplicateEntity Persistent grabbedID
            --        --forkCode grabbedID duplicateID
            --        grabEntity handEntityID duplicateID
            | not wantsToHandleDrag ->
                grabEntity handEntityID grabbedID

grabEntity :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> m ()
grabEntity handEntityID grabbedID = do
    selectEntity grabbedID
    attachEntityToEntity handEntityID grabbedID True
