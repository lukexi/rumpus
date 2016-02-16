{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Rumpus.Systems.SceneEditor where
import PreludeExtra

import Data.ECS
import Rumpus.Types
import Rumpus.Systems.Controls
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Attachment
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Selection
import Rumpus.Systems.Constraint

data Scene = Scene
    { _scnName     :: !String
    -- , _scnEntities :: !(Map EntityID Entity)
    }
makeLenses ''Scene

newScene :: Scene
newScene = Scene 
    { _scnName = "NewScene"
    -- , _scnEntities = mempty 
    }

data SceneEditorSystem = SceneEditorSystem
    { _sedScene              :: !Scene
    , _sedCurrentEditorFrame :: !(Maybe EntityID)
    -- , _sedEntityLibrary      :: !(Map String Entity)
    }
makeLenses ''SceneEditorSystem
defineSystemKey ''SceneEditorSystem

-- data Drag = Drag HandEntityID (Pose GLfloat)
data Drag = Drag HandEntityID (V3 GLfloat)

type OnDrag = EntityID -> V3 GLfloat -> ECSMonad ()

defineComponentKey ''Drag
defineComponentKey ''OnDrag

initSceneEditorSystem :: MonadState ECS m => m ()
initSceneEditorSystem = do
    registerSystem sysSceneEditor $ SceneEditorSystem newScene Nothing

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
        removeComponent cmpShapeType =<< ask
        cmpConstraint ==> (RelativePositionTo entityID 0)
    
    ------------------------
    -- Define a color editor
    _colorEditor <- spawnEntity Transient $ do
        cmpParent            ==> editorFrame
        cmpShapeType         ==> SphereShape
        cmpColor             ==> V4 1 0 0 1
        cmpSize              ==> 0.1
        cmpPhysicsProperties ==> [IsKinematic, NoContactResponse]
        cmpConstraint        ==> RelativePositionTo editorFrame (V3 (-0.5) 0.5 0)
        cmpOnDrag            ==> \_colorEditorID dragDistance -> do
            let x = dragDistance ^. _x
            setEntityColor (hslColor (mod' x 1) 0.9 0.6 1) entityID

    -----------------------
    -- Define a size editor

    _sizeEditor <- spawnEntity Transient $ do
        cmpParent            ==> editorFrame
        cmpShapeType         ==> SphereShape
        cmpColor             ==> V4 0 1 0 1
        cmpSize              ==> 0.1
        cmpPhysicsProperties ==> [IsKinematic, NoContactResponse]
        cmpConstraint        ==> RelativePositionTo editorFrame (V3 0.5 0.5 0)
        cmpOnDrag            ==> \_sizeEditorID dragDistance -> do
            let size = max 0.05 (abs dragDistance)
            setEntitySize size entityID

    modifySystemState sysSceneEditor $ sedCurrentEditorFrame ?= editorFrame

    return ()

beginDrag :: (MonadState ECS m, MonadIO m) => EntityID -> EntityID -> m ()
beginDrag handEntityID draggedID = do
    startPos <- view posPosition <$> getEntityPose handEntityID
    setComponent cmpDrag (Drag handEntityID startPos) draggedID

continueDrag :: HandEntityID -> ECSMonad ()
continueDrag draggingHandEntityID = do
    forEntitiesWithComponent cmpDrag $ \(entityID, Drag handEntityID startPos) ->
        when (handEntityID == draggingHandEntityID) $ do
            currentPose <- view posPosition <$> getEntityPose handEntityID
            let dragDistance = currentPose - startPos

            withComponent entityID cmpOnDrag $ \onDrag ->
                onDrag entityID dragDistance

endDrag :: MonadState ECS m => HandEntityID -> m ()
endDrag endingDragHandEntityID = do
    forEntitiesWithComponent cmpDrag $ \(entityID, Drag handEntityID _) ->
        when (handEntityID == endingDragHandEntityID) $
            removeComponent cmpDrag entityID


tickSceneEditorSystem :: ECSMonad ()
tickSceneEditorSystem = do
    let editSceneWithHand handName event = do
            mHandEntityID <- listToMaybe <$> getEntityIDsWithName handName
            forM_ mHandEntityID $ \handEntityID -> case event of
                HandStateEvent hand -> do
                    setEntityPose (poseFromMatrix (hand ^. hndMatrix)) handEntityID
                    continueDrag handEntityID
                HandButtonEvent HandButtonGrip ButtonDown -> do
                    handPose <- getEntityPose handEntityID
                    _ <- spawnEntity Persistent $ do
                        cmpPose          ==> handPose 
                        cmpShapeType     ==> CubeShape
                        cmpSize          ==> 0.5
                        -- cmpOnUpdateExpr  ==> ("scenes/minimal/DefaultUpdate.hs", "update")
                    
                    return ()
                HandButtonEvent HandButtonTrigger ButtonDown -> do

                    didPlaceCursor <- raycastCursor handEntityID
                    when (not didPlaceCursor) $ do
                        -- Find the entities overlapping the hand, and attach them to it
                        overlappingEntityIDs <- filterM (fmap (not . elem Static) . getEntityPhysProps)
                                                    =<< getEntityOverlappingEntityIDs handEntityID
                        -- printIO overlappingEntityIDs
                        when (null overlappingEntityIDs) clearSelection
                        
                        forM_ (listToMaybe overlappingEntityIDs) $ \touchedID -> do

                            -- See if the touched object has the current EditorFrame as a parent;
                            -- If so, it's a draggable object.
                            -- (we should just look to see if it has a drag function, actually!)
                            currentEditorFrame <- viewSystem sysSceneEditor sedCurrentEditorFrame
                            touchedParentID <- getComponent touchedID cmpParent
                            if (isJust currentEditorFrame && currentEditorFrame == touchedParentID) 
                                then do
                                    beginDrag handEntityID touchedID
                                else do
                                    selectEntity touchedID

                                    attachEntity handEntityID touchedID
                HandButtonEvent HandButtonTrigger ButtonUp -> do
                    endDrag handEntityID
                    detachEntity handEntityID

                    -- If we've selected something, show the keyboard on grip-up
                    traverseM_ (viewSystem sysSelection selSelectedEntityID) $ \_selectedID -> do
                        vrPal <- viewSystem sysControls ctsVRPal
                        showHandKeyboard vrPal

                    
                    saveEntities

                _ -> return ()

    withLeftHandEvents  (editSceneWithHand "Left Hand")
    withRightHandEvents (editSceneWithHand "Right Hand")


sceneFileNamed :: String -> FilePath
sceneFileNamed sceneName = "scenes" </> sceneName </> "scene.yaml"

