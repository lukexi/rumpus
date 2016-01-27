{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Rumpus.Systems.SceneEditor where
import PreludeExtra

import Rumpus.Types
import Rumpus.Control
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Attachment
import Rumpus.Systems.Script
import Rumpus.Systems.Sound
import Rumpus.Systems.Lifetime
import Rumpus.Systems.Constraint

import Data.Yaml hiding ((.=))
import qualified Data.Map as Map

clearSelection :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => m ()
clearSelection = do
    useTraverseM_ wldCurrentEditorFrame removeEntity
    wldSelectedEntityID .= Nothing


selectEntity :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => EntityID -> m ()
selectEntity entityID = do

    clearSelection

    wldSelectedEntityID ?= entityID


    let editorFrameEntity = newEntity
            -- & entShape .~ CubeShape
            & entSize  .~ 1
    editorFrame <- createEntity Transient editorFrameEntity

    setEntityConstraint (RelativePositionTo entityID 0) editorFrame

    -- Define a color editor
    let colorEditorEntity = newEntity
            & entShape .~ SphereShape
            & entColor .~ V4 1 0 0 1
            & entSize .~ 0.1
            & entPhysicsProperties .~ [IsKinematic, NoContactResponse]
    colorEditor <- createEntity Transient colorEditorEntity

    wldComponents . cmpOnDrag . at colorEditor ?= \_colorEditorID dragDistance -> do
        let x = dragDistance ^. _x
        setEntityColor (hslColor (mod' x 1) 0.9 0.9 1) entityID

    setEntityConstraint (RelativePositionTo editorFrame (V3 0.5 0.5 0)) colorEditor

    addEntityChild editorFrame colorEditor

    -- Define a size editor
    let sizeEditorEntity = newEntity
            & entShape .~ SphereShape
            & entColor .~ V4 0 1 0 1
            & entSize .~ 0.1
            & entPhysicsProperties .~ [IsKinematic, NoContactResponse]
    sizeEditor <- createEntity Transient sizeEditorEntity

    wldComponents . cmpOnDrag . at sizeEditor ?= \_sizeEditorID dragDistance -> do
        let size = max 0.05 (abs dragDistance)
        setEntitySize size entityID

    setEntityConstraint (RelativePositionTo editorFrame (V3 0.5 (-0.5) 0)) sizeEditor
    
    addEntityChild editorFrame sizeEditor

    wldCurrentEditorFrame ?= editorFrame

    -- Tick the constraint system once to put things in place for this frame
    constraintSystem

    return ()

addEntityChild :: MonadState World m => EntityID -> EntityID -> m ()
addEntityChild entityID childEntityID = do
    wldComponents . cmpParent . at childEntityID ?= entityID

beginDrag :: MonadState World m => EntityID -> EntityID -> m ()
beginDrag handEntityID draggedID = do
    startPos <- view posPosition <$> getEntityPose handEntityID
    wldComponents . cmpDrag . at draggedID ?= Drag handEntityID startPos

continueDrag :: HandEntityID -> WorldMonad ()
continueDrag draggingHandEntityID = do
    useMapM_ (wldComponents . cmpDrag) $ \(entityID, Drag handEntityID startPos) ->
        when (handEntityID == draggingHandEntityID) $ do
            currentPose <- view posPosition <$> getEntityPose handEntityID
            let dragDistance = currentPose - startPos

            useTraverseM_ (wldComponents . cmpOnDrag . at entityID) $ \onDrag ->
                onDrag entityID dragDistance

endDrag :: MonadState World m => HandEntityID -> m ()
endDrag endingDragHandEntityID = do
    useMapM_ (wldComponents . cmpDrag) $ \(entityID, Drag handEntityID _) ->
        when (handEntityID == endingDragHandEntityID) $
            wldComponents . cmpDrag . at entityID .= Nothing


sceneEditorSystem :: WorldMonad ()
sceneEditorSystem = do
    let editSceneWithHand handName event = do
            mHandEntityID <- listToMaybe <$> getEntityIDsWithName handName
            forM_ mHandEntityID $ \handEntityID -> case event of
                HandStateEvent hand -> do
                    setEntityPose (poseFromMatrix (hand ^. hndMatrix)) handEntityID
                    continueDrag handEntityID
                HandButtonEvent HandButtonGrip ButtonDown -> do
                    handPose <- getEntityPose handEntityID
                    let entity = newEntity 
                            & entPose .~ handPose 
                            & entShape .~ CubeShape
                            & entSize .~ 0.5
                    _ <- createEntity Persistent entity
                    return ()
                HandButtonEvent HandButtonTrigger ButtonDown -> do
                    
                    -- Find the entities overlapping the hand, and attach them to it
                    overlappingEntityIDs <- filterM (fmap (/= "Floor") . getEntityName) 
                                                =<< getEntityOverlappingEntityIDs handEntityID
                    -- printIO overlappingEntityIDs
                    when (null overlappingEntityIDs) clearSelection
                    
                    forM_ (listToMaybe overlappingEntityIDs) $ \touchedID -> do

                        currentEditorFrame <- use wldCurrentEditorFrame
                        touchedParentID <- use (wldComponents . cmpParent . at touchedID)
                        if (isJust currentEditorFrame && currentEditorFrame == touchedParentID) 
                            then do
                                beginDrag handEntityID touchedID
                            else do
                                -- Select the entity (it's ok to select the floor, just not move it)
                                selectEntity touchedID

                                name <- getEntityName touchedID
                                when (name /= "Floor") $ 
                                    attachEntity handEntityID touchedID
                HandButtonEvent HandButtonTrigger ButtonUp -> do
                    endDrag handEntityID
                    -- Copy the current pose to the scene file and save it
                    withAttachment handEntityID $ \(Attachment attachedEntityID _offset) -> do
                        currentPose <- getEntityPose attachedEntityID
                        wldScene . scnEntities . at attachedEntityID . traverse . entPose .= currentPose
                        saveScene
                    detachEntity handEntityID
                _ -> return ()

    withLeftHandEvents (editSceneWithHand "Left Hand")
    withRightHandEvents (editSceneWithHand "Right Hand")

sceneFileNamed :: String -> FilePath
sceneFileNamed sceneName = "scenes" </> sceneName </> "scene.yaml"

loadScene :: (MonadReader WorldStatic m, MonadState World m, MonadIO m) => FilePath -> m ()
loadScene sceneName =     
    liftIO (decodeFileEither (sceneFileNamed sceneName)) >>= \case
        Left parseException -> putStrLnIO ("Error loading " ++ sceneName ++ ": " ++ show parseException)
        Right entities -> do
            wldScene .= Scene { _scnName = sceneName, _scnEntities = Map.fromList entities }
            forM_ (entities :: [(EntityID, Entity)]) $ \(entityID, entity) -> do
                defineEntity entity
                createEntityWithID Persistent entityID entity

saveScene :: (MonadState World m, MonadIO m) => m ()
saveScene = do
    sceneName     <- use (wldScene . scnName)
    sceneEntities <- use (wldScene . scnEntities)
    let sceneFile = sceneFileNamed sceneName
    liftIO $ encodeFile sceneFile (Map.toList sceneEntities)



spawnEntity :: (MonadReader WorldStatic m, MonadState World m, MonadIO m) => Persistence -> String -> m (Maybe EntityID)
spawnEntity persistence entityName = 
    traverseM (use (wldEntityLibrary . at entityName)) (createEntity persistence)

defineEntity :: MonadState World m => Entity -> m ()
defineEntity entity = wldEntityLibrary . at (entity ^. entName) ?= entity

createEntity :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
             => Persistence -> Entity -> m EntityID
createEntity persistence entity = do
    entityID <- liftIO randomIO
    createEntityWithID persistence entityID entity

createEntityWithID :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
                   => Persistence -> EntityID -> Entity -> m EntityID
createEntityWithID persistence entityID entity = do
    when (persistence == Persistent) $
        wldScene . scnEntities . at entityID ?= entity

    wldComponents . cmpPose  . at entityID ?= entity ^. entPose
    wldComponents . cmpSize  . at entityID ?= entity ^. entSize
    wldComponents . cmpColor . at entityID ?= entity ^. entColor
    wldComponents . cmpShape . at entityID ?= entity ^. entShape
    wldComponents . cmpName  . at entityID ?= entity ^. entName

    addScriptComponent   entityID entity
    addPhysicsComponent  entityID entity
    addPdPatchComponent  entityID entity
    addLifetimeComponent entityID entity

    forM_ (entity ^. entChildren) $ \child -> do
        childID <- createEntity persistence child
        wldComponents . cmpParent . at childID ?= entityID
    
    return entityID

