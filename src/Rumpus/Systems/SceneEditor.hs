{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.SceneEditor where
import PreludeExtra

import Rumpus.Types
import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Sound
import Rumpus.Systems.Attachment
--import Rumpus.Systems.CodeEditor
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

    --vrPal <- viewSystem sysControls ctsVRPal
    --hideHandKeyboard vrPal

    removeCurrentEditorFrame
    
    clearSelectedEntityID


selectEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
selectEntity entityID = do

    clearSelection

    setSelectedEntityID entityID

    --addEditorFrame entityID

    return ()

removeCurrentEditorFrame :: (MonadIO m, MonadState ECS m) => m ()
removeCurrentEditorFrame = traverseM_ (viewSystem sysSceneEditor sedCurrentEditorFrame) removeEntity

addEditorFrame :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
addEditorFrame entityID = do
    editorFrame <- spawnEntity Transient $ do
        removeComponent cmpShapeType
        cmpConstraint ==> RelativePositionTo entityID 0
    
    ------------------------
    -- Define a color editor
    color <- getEntityColor entityID
    _colorEditor <- spawnEntity Transient $ do
        cmpParent            ==> editorFrame
        cmpShapeType         ==> SphereShape
        cmpColor             ==> color
        cmpSize              ==> 0.1
        cmpPhysicsProperties ==> [Kinematic, NoContactResponse]
        cmpConstraint        ==> RelativePositionTo editorFrame (V3 (-0.5) 0.5 0)
        --cmpPose              ==> (newPose & posPosition .~ V3 (-0.5) 0.5 0)
        cmpOnDrag            ==> \dragDistance -> do
            let x = dragDistance ^. _x
                newColor = hslColor (mod' x 1) 0.9 0.6
            setColor newColor
            setEntityColor newColor entityID

    -----------------------
    -- Define a size editor
    
    _sizeEditor <- spawnEntity Transient $ do
        cmpParent            ==> editorFrame
        cmpShapeType         ==> CubeShape
        cmpColor             ==> V4 0.3 0.3 1 1
        cmpSize              ==> 0.2
        cmpPhysicsProperties ==> [Kinematic, NoContactResponse]
        cmpConstraint        ==> RelativePositionTo editorFrame (V3 0.5 0.5 0)
        --cmpPose              ==> (newPose & posPosition .~ V3 0.5 0.5 0)
        cmpOnDrag            ==> \dragDistance -> do
            let size = max 0.05 (abs dragDistance)
            -- Set the edited entity's size, not the editor-widget's : )
            setEntitySize size entityID

    modifySystemState sysSceneEditor $ 
        sedCurrentEditorFrame ?= editorFrame 

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
    let editSceneWithHand handEntityID otherHandEntityID event = case event of
            HandStateEvent hand -> do
                setEntityPose (hand ^. hndMatrix) handEntityID
                continueDrag handEntityID
            HandButtonEvent HandButtonGrip ButtonDown -> do
                --handPose <- getEntityPose handEntityID
                --_ <- spawnNewEntityAtPose handPose

                handPose <- getEntityPose handEntityID

                let handRay = poseToRay (poseFromMatrix handPose) (V3 0 0 (-1)) :: Ray GLfloat

                mRayResult <- castRay handRay
                forM_ mRayResult $ \RayResult{..} -> do
                    entityID <- unCollisionObjectID <$>  getCollisionObjectID rrCollisionObject

                    teleportable <- getIsTeleportable entityID
                    when teleportable $ do
                        pose          <- getEntityPose entityID
                        V3 _ height _ <- getEntitySize entityID
                        let V3 x y z = pose ^. translation
                        setPlayerPosition (V3 x (y+height/2) z)


                return ()
            HandButtonEvent HandButtonTrigger ButtonDown -> do

                --didPlaceCursor <- raycastCursor handEntityID
                let didPlaceCursor = False
                when (not didPlaceCursor) $ do
                    -- Find the entities overlapping the hand, and attach them to it
                    overlappingEntityIDs <- filterStaticEntityIDs
                                                =<< getEntityOverlappingEntityIDs handEntityID

                    when (null overlappingEntityIDs) clearSelection
                    
                    forM_ (listToMaybe overlappingEntityIDs) $ \grabbedID -> do

                        hasDragFunction <- entityHasComponent grabbedID cmpOnDrag
                        isBeingHeldByOtherHand <- isEntityAttachedTo grabbedID otherHandEntityID
                        if 
                            | isBeingHeldByOtherHand -> do
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
                endDrag handEntityID
                detachAttachedEntities handEntityID

                -- Saving is currently disabled to simplify the alpha release
                -- (code will still be saved automatically)
                --saveScene
                
            _ -> return ()

    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    withLeftHandEvents  (editSceneWithHand leftHandID  rightHandID)
    withRightHandEvents (editSceneWithHand rightHandID leftHandID)


filterStaticEntityIDs :: MonadState ECS m => [EntityID] -> m [EntityID]
filterStaticEntityIDs = filterM (fmap (not . elem Static) . getEntityPhysicsProperties)

getIsTeleportable :: MonadState ECS m => EntityID -> m Bool
getIsTeleportable = fmap (elem Teleportable) . getEntityPhysicsProperties

loadScene :: (MonadIO m, MonadState ECS m) => String -> m ()
loadScene sceneFolder = do
    putStrLnIO $ "Loading scene: " ++ sceneFolder
    modifySystemState sysSelection (selScene . scnFolder .= sceneFolder)
    addPdPatchSearchPath sceneFolder
    loadEntities sceneFolder

saveScene :: ECSMonad ()
saveScene = do
    sceneFolder <- viewSystem sysSelection (selScene . scnFolder)
    saveEntities sceneFolder