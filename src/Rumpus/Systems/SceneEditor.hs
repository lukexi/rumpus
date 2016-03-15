{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Rumpus.Systems.SceneEditor where
import PreludeExtra

import Rumpus.Types
import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Attachment
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Selection
import Rumpus.Systems.Constraint

data SceneEditorSystem = SceneEditorSystem
    { _sedCurrentEditorFrame :: !(Maybe EntityID)
    }
makeLenses ''SceneEditorSystem
defineSystemKey ''SceneEditorSystem

-- data Drag = Drag HandEntityID (Pose GLfloat)
data Drag = Drag HandEntityID (V3 GLfloat)

type OnDrag = V3 GLfloat -> EntityMonad ()

defineComponentKey ''Drag
defineComponentKey ''OnDrag

initSceneEditorSystem :: MonadState ECS m => m ()
initSceneEditorSystem = do
    registerSystem sysSceneEditor $ SceneEditorSystem Nothing

    registerComponent "Drag"   cmpDrag   (newComponentInterface cmpDrag)
    registerComponent "OnDrag" cmpOnDrag (newComponentInterface cmpOnDrag)

clearSelection :: (MonadIO m, MonadState ECS m) => m ()
clearSelection = do

    vrPal <- viewSystem sysControls ctsVRPal
    hideHandKeyboard vrPal

    traverseM_ (viewSystem sysSceneEditor sedCurrentEditorFrame) removeEntity
    
    modifySystem_ sysSelection $ return . (selSelectedEntityID .~ Nothing)


selectEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
selectEntity entityID = do

    clearSelection

    modifySystemState sysSelection $ selSelectedEntityID ?= entityID

    editorFrame <- spawnEntity Transient $ do
        removeComponent cmpShapeType
        cmpConstraint ==> (RelativePositionTo entityID 0)
    
    ------------------------
    -- Define a color editor
    color <- getEntityColor entityID
    _colorEditor <- spawnEntity Transient $ do
        cmpParent            ==> editorFrame
        cmpShapeType         ==> SphereShape
        cmpColor             ==> color
        cmpSize              ==> 0.1
        cmpPhysicsProperties ==> [IsKinematic, NoContactResponse]
        cmpConstraint        ==> RelativePositionTo editorFrame (V3 (-0.5) 0.5 0)
        --cmpPose              ==> (newPose & posPosition .~ V3 (-0.5) 0.5 0)
        cmpOnDrag            ==> \dragDistance -> do
            let x = dragDistance ^. _x
                newColor = hslColor (mod' x 1) 0.9 0.6 1
            setColor newColor
            setEntityColor newColor entityID

    -----------------------
    -- Define a size editor
    
    _sizeEditor <- spawnEntity Transient $ do
        cmpParent            ==> editorFrame
        cmpShapeType         ==> CubeShape
        cmpColor             ==> V4 0.3 0.3 1 1
        cmpSize              ==> 0.2
        cmpPhysicsProperties ==> [IsKinematic, NoContactResponse]
        cmpConstraint        ==> RelativePositionTo editorFrame (V3 0.5 0.5 0)
        --cmpPose              ==> (newPose & posPosition .~ V3 0.5 0.5 0)
        cmpOnDrag            ==> \dragDistance -> do
            let size = max 0.05 (abs dragDistance)
            -- Set the edited entity's size, not the editor-widget's : )
            setEntitySize size entityID

    modifySystemState sysSceneEditor $ sedCurrentEditorFrame ?= editorFrame

    return ()

beginDrag :: (MonadState ECS m, MonadIO m) => EntityID -> EntityID -> m ()
beginDrag handEntityID draggedID = do
    startPos <- view translation <$> getEntityPose handEntityID
    setEntityComponent cmpDrag (Drag handEntityID startPos) draggedID

continueDrag :: HandEntityID -> ECSMonad ()
continueDrag draggingHandEntityID = do
    forEntitiesWithComponent cmpDrag $ \(entityID, Drag handEntityID startPos) ->
        when (handEntityID == draggingHandEntityID) $ do
            currentPose <- view translation <$> getEntityPose handEntityID
            let dragDistance = currentPose - startPos

            runEntity entityID $ 
                withComponent_ cmpOnDrag $ \onDrag ->
                    onDrag dragDistance

endDrag :: MonadState ECS m => HandEntityID -> m ()
endDrag endingDragHandEntityID = do
    forEntitiesWithComponent cmpDrag $ \(entityID, Drag handEntityID _) -> runEntity entityID $ 
        when (handEntityID == endingDragHandEntityID) $
            removeComponent cmpDrag

spawnNewEntityAtPose :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m EntityID
spawnNewEntityAtPose pose = spawnEntity Persistent $ do
    cmpPose          ==> pose 
    cmpShapeType     ==> CubeShape
    cmpSize          ==> 0.5
    -- cmpOnUpdateExpr  ==> ("scenes/minimal/DefaultUpdate.hs", "update")

tickSceneEditorSystem :: ECSMonad ()
tickSceneEditorSystem = do
    let editSceneWithHand handEntityID event = case event of
                HandStateEvent hand -> do
                    setEntityPose (hand ^. hndMatrix) handEntityID
                    continueDrag handEntityID
                HandButtonEvent HandButtonGrip ButtonDown -> do
                    handPose <- getEntityPose handEntityID
                    _ <- spawnNewEntityAtPose handPose
                    return ()
                HandButtonEvent HandButtonTrigger ButtonDown -> do

                    didPlaceCursor <- raycastCursor handEntityID
                    when (not didPlaceCursor) $ do
                        -- Find the entities overlapping the hand, and attach them to it
                        overlappingEntityIDs <- filterStaticEntityIDs
                                                    =<< getEntityOverlappingEntityIDs handEntityID
                        -- printIO overlappingEntityIDs
                        when (null overlappingEntityIDs) clearSelection
                        
                        forM_ (listToMaybe overlappingEntityIDs) $ \touchedID -> do

                            hasDragFunction <- entityHasComponent touchedID cmpOnDrag
                            if hasDragFunction
                                then do
                                    beginDrag handEntityID touchedID
                                else do
                                    --selectEntity touchedID

                                    attachEntity handEntityID touchedID
                HandButtonEvent HandButtonTrigger ButtonUp -> do
                    endDrag handEntityID
                    detachEntity handEntityID

                    -- If we've selected something, show the keyboard on grip-up
                    traverseM_ (viewSystem sysSelection selSelectedEntityID) $ \_selectedID -> do
                        -- vrPal <- viewSystem sysControls ctsVRPal
                        -- showHandKeyboard vrPal
                        return ()

                    -- WARNING!!! 
                    -- NOT SAVING TO AVOID SCREWING UP DEMO!!
                    --saveScene

                _ -> return ()

    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    withLeftHandEvents  (editSceneWithHand leftHandID)
    withRightHandEvents (editSceneWithHand rightHandID)

filterStaticEntityIDs :: MonadState ECS m => [EntityID] -> m [EntityID]
filterStaticEntityIDs = filterM (fmap (not . elem Static) . getEntityPhysicsProperties)


