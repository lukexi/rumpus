{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Rumpus.Systems.KeyboardHands
import Rumpus.Systems.Haptics
import Rumpus.Systems.Selection

data SceneEditorSystem = SceneEditorSystem
    { _sedCurrentEditorFrame :: !(Maybe EntityID)
    }
makeLenses ''SceneEditorSystem
defineSystemKey ''SceneEditorSystem

-- data Drag = Drag HandEntityID (M44 GLfloat)
data Drag = Drag HandEntityID (V3 GLfloat)

type OnDrag = V3 GLfloat -> EntityMonad ()

defineComponentKey ''Drag
defineComponentKey ''OnDrag

initSceneEditorSystem :: MonadState ECS m => m ()
initSceneEditorSystem = do
    registerSystem sysSceneEditor $ SceneEditorSystem Nothing

    registerComponent "Drag"   myDrag   (newComponentInterface myDrag)
    registerComponent "OnDrag" myOnDrag (newComponentInterface myOnDrag)

clearSelection :: (MonadIO m, MonadState ECS m) => m ()
clearSelection = do

    hideKeyboardHands

    removeCurrentEditorFrame
    
    clearSelectedEntityID


selectEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
selectEntity entityID = do

    clearSelection

    setSelectedEntityID entityID

    showKeyboardHands
    --addEditorFrame entityID

    return ()

removeCurrentEditorFrame :: (MonadIO m, MonadState ECS m) => m ()
removeCurrentEditorFrame = traverseM_ (viewSystem sysSceneEditor sedCurrentEditorFrame) removeEntity



beginDrag :: (MonadState ECS m, MonadIO m) => EntityID -> EntityID -> m ()
beginDrag handEntityID draggedID = do
    startPos <- view translation <$> getEntityPose handEntityID
    setEntityComponent myDrag (Drag handEntityID startPos) draggedID

continueDrag :: HandEntityID -> ECSMonad ()
continueDrag draggingHandEntityID = do
    forEntitiesWithComponent myDrag $ \(entityID, Drag handEntityID startPos) ->
        when (handEntityID == draggingHandEntityID) $ do
            currentPose <- view translation <$> getEntityPose handEntityID
            let dragDistance = currentPose - startPos

            runEntity entityID $ 
                withComponent_ myOnDrag $ \onDrag ->
                    onDrag dragDistance

endDrag :: MonadState ECS m => HandEntityID -> m ()
endDrag endingDragHandEntityID = do
    forEntitiesWithComponent myDrag $ \(entityID, Drag handEntityID _) -> do
        when (handEntityID == endingDragHandEntityID) $ do

            runEntity entityID $ removeComponent myDrag

spawnNewEntityAtPose :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m EntityID
spawnNewEntityAtPose pose = spawnEntity $ do
    myPose          ==> pose 
    myShapeType     ==> CubeShape
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
                        handPose <- getEntityPose handEntityID
                        beginHapticDrag whichHand handPose

                        hasDragFunction        <- entityHasComponent grabbedID myOnDrag
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