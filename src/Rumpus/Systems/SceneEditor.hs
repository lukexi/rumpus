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



data Persistence = Transient | Persistent 
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data SceneEditorSystem = SceneEditorSystem
    { _sedScene              :: !Scene
    , _sedCurrentEditorFrame :: !(Maybe EntityID)
    -- , _sesEntityLibrary      :: !(Map String Entity)
    }
makeLenses ''SceneEditorSystem
defineSystemKey ''SceneEditorSystem

-- data Drag = Drag HandEntityID (Pose GLfloat)
data Drag = Drag HandEntityID (V3 GLfloat)

-- | OnDrag function
type OnDrag = EntityID -> V3 GLfloat -> ECSMonad ()
nullOnDrag :: OnDrag
nullOnDrag _entityID _dragDistance = return ()

defineComponentKey ''Drag
defineComponentKey ''OnDrag


clearSelection :: (MonadIO m, MonadState ECS m) => m ()
clearSelection = do

    vrPal <- viewSystem sysControls ctsVRPal
    hideHandKeyboard vrPal

    traverseM_ (viewSystem sysSceneEditor sedCurrentEditorFrame) removeEntity
    
    modifySystem_ sysSelection $ return . (selSelectedEntityID .~ Nothing)


selectEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
selectEntity entityID = do

    clearSelection

    modifySystem_ sysSelection $ return . (selSelectedEntityID ?~ entityID)

    {- FIXME
    let editorFrameEntity = newEntity
            -- & entShape .~ CubeShape
            & entSize  .~ 1
    editorFrame <- createEntity Transient editorFrameEntity

    setEntityConstraint (RelativePositionTo entityID 0) editorFrame

    ------------------------
    -- Define a color editor
    let colorEditorEntity = newEntity
            & entShape .~ SphereShape
            & entColor .~ V4 1 0 0 1
            & entSize .~ 0.1
            & entPhysicsProperties .~ [IsKinematic, NoContactResponse]
    colorEditor <- createEntity Transient colorEditorEntity

    wldComponents . cmpOnDrag . at colorEditor ?= \_colorEditorID dragDistance -> do
        let x = dragDistance ^. _x
        setEntityColor (hslColor (mod' x 1) 0.9 0.6 1) entityID

    setEntityConstraint (RelativePositionTo editorFrame (V3 (-0.5) 0.5 0)) colorEditor

    addEntityChild editorFrame colorEditor

    -----------------------
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

    setEntityConstraint (RelativePositionTo editorFrame (V3 0.5 0.5 0)) sizeEditor
    
    addEntityChild editorFrame sizeEditor

    modifySystem_ sysSceneEditor $ sedCurrentEditorFrame ?~ editorFrame

    -- Tick the constraint system once to put things in place for this frame
    constraintSystem
    -}

    return ()

addEntityChild :: (MonadState ECS m, MonadIO m) => EntityID -> EntityID -> m ()
addEntityChild entityID childEntityID = 
    addComponent cmpParent entityID childEntityID


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
                    {- FIXME
                    handPose <- getEntityPose handEntityID
                    let entity = newEntity 
                            & entPose .~ handPose 
                            & entShape .~ CubeShape
                            & entSize .~ 0.5
                            & entOnUpdate ?~ "scenes/minimal/DefaultUpdate.hs"
                    _ <- createEntity Persistent entity
                    -}
                    return ()
                HandButtonEvent HandButtonTrigger ButtonDown -> do

                    didPlaceCursor <- raycastCursor handEntityID
                    when (not didPlaceCursor) $ do
                        -- Find the entities overlapping the hand, and attach them to it
                        overlappingEntityIDs <- filterM (fmap (/= "Floor") . getEntityName) 
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
                                    -- Select the entity (it's ok to select the floor, just not move it)
                                    selectEntity touchedID

                                    name <- getEntityName touchedID
                                    when (name /= "Floor") $ 
                                        attachEntity handEntityID touchedID
                HandButtonEvent HandButtonTrigger ButtonUp -> do
                    endDrag handEntityID
                    detachEntity handEntityID

                    -- If we've selected something, show the keyboard on grip-up
                    traverseM_ (viewSystem sysSelection selSelectedEntityID) $ \_selectedID -> do
                        vrPal <- viewSystem sysControls ctsVRPal
                        showHandKeyboard vrPal

                    -- useTraverseM_ wldSelectedEntityID 
                    --     updateEntityInScene
                    -- saveScene

                _ -> return ()

    withLeftHandEvents  (editSceneWithHand "Left Hand")
    withRightHandEvents (editSceneWithHand "Right Hand")

-- updateEntityInScene :: MonadState ECS m => EntityID -> m ()
-- updateEntityInScene entityID = do
--     -- Copy the current pose to the scene file and save it
--     pose <- getEntityPose entityID
--     wldScene . scnEntities . at entityID . traverse . entPose .= pose
--     color <- getEntityColor entityID
--     wldScene . scnEntities . at entityID . traverse . entColor .= color
--     size <- getEntitySize entityID
--     wldScene . scnEntities . at entityID . traverse . entSize .= size

sceneFileNamed :: String -> FilePath
sceneFileNamed sceneName = "scenes" </> sceneName </> "scene.yaml"

{- FIXME
loadScene :: (MonadState ECS m, MonadIO m) => FilePath -> m ()
loadScene sceneName =     
    liftIO (decodeFileEither (sceneFileNamed sceneName)) >>= \case
        Left parseException -> putStrLnIO ("Error loading " ++ sceneName ++ ": " ++ show parseException)
        Right entities -> do
            wldScene .= Scene { _scnName = sceneName, _scnEntities = Map.fromList entities }
            forM_ (entities :: [(EntityID, Entity)]) $ \(entityID, entity) -> do
                defineEntity entity
                createEntityWithID Persistent entityID entity

saveScene :: (MonadState ECS m, MonadIO m) => m ()
saveScene = do
    sceneName     <- use (wldScene . scnName)
    sceneEntities <- use (wldScene . scnEntities)
    let sceneFile = sceneFileNamed sceneName
    liftIO $ encodeFile sceneFile (Map.toList sceneEntities)



spawnEntity :: (MonadState ECS m, MonadIO m) => Persistence -> String -> m (Maybe EntityID)
spawnEntity persistence entityName = 
    traverseM (use (wldEntityLibrary . at entityName)) (createEntity persistence)

defineEntity :: MonadState ECS m => Entity -> m ()
defineEntity entity = wldEntityLibrary . at (entity ^. entName) ?= entity

createEntity :: (MonadIO m, MonadState ECS m) 
             => Persistence -> Entity -> m EntityID
createEntity persistence entity = do
    entityID <- liftIO randomIO
    createEntityWithID persistence entityID entity

createEntityWithID :: (MonadIO m, MonadState ECS m) 
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
-}
