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

import Data.Yaml hiding ((.=))
import qualified Data.Map as Map


sceneEditorSystem :: WorldMonad ()
sceneEditorSystem = do
    let editSceneWithHand handName event = do
            mHandEntityID <- listToMaybe <$> getEntityIDsWithName handName
            forM_ mHandEntityID $ \handEntityID -> case event of
                HandStateEvent hand -> 
                    setEntityPose (poseFromMatrix (hand ^. hndMatrix)) handEntityID
                HandButtonEvent HandButtonGrip ButtonDown -> do
                    handPose <- getEntityPose handEntityID
                    let entity = newEntity & entPose .~ handPose & entShape .~ CubeShape
                    _ <- createEntity Persistent entity
                    return ()
                HandButtonEvent HandButtonTrigger ButtonDown -> do
                    -- Find the entities overlapping the hand, and attach them to it
                    overlappingEntityIDs <- filterM (fmap (/= "Floor") . getEntityName) 
                                            =<< getEntityGhostOverlappingEntityIDs handEntityID
                    forM_ (listToMaybe overlappingEntityIDs) $ \touchedID -> do
                        -- Select the entity (it's ok to select the floor, just not move it)
                        wldSelectedEntityID ?= touchedID

                        name <- getEntityName touchedID
                        when (name /= "Floor") $ 
                            attachEntity handEntityID touchedID
                HandButtonEvent HandButtonTrigger ButtonUp -> do
                    -- Copy the current pose to the scene file and save it
                    withAttachment handEntityID $ \(Attachment attachedEntityID _offset) -> do
                        currentPose <- getEntityPose attachedEntityID
                        wldScene . scnEntities . at attachedEntityID . traverse . entPose .= currentPose
                        saveScene
                    detachEntity handEntityID
                _ -> return ()

    withLeftHandEvents (editSceneWithHand "Left Hand")
    withRightHandEvents (editSceneWithHand "Right Hand")


loadSceneFile :: (MonadReader WorldStatic m, MonadState World m, MonadIO m) => FilePath -> m ()
loadSceneFile sceneFile =     
    liftIO (decodeFileEither sceneFile) >>= \case
        Left parseException -> putStrLnIO ("Error loading " ++ sceneFile ++ ": " ++ show parseException)
        Right entities -> do
            wldScene .= Scene { _scnName = sceneFile, _scnEntities = Map.fromList entities }
            forM_ (entities :: [(EntityID, Entity)]) $ \(entityID, entity) -> do
                defineEntity entity
                createEntityWithID Persistent entityID entity

saveScene :: (MonadState World m, MonadIO m) => m ()
saveScene = do
    sceneFile     <- use (wldScene . scnName)
    sceneEntities <- use (wldScene . scnEntities)
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
    wldComponents . cmpScale . at entityID ?= entity ^. entScale
    wldComponents . cmpShape . at entityID ?= entity ^. entShape
    wldComponents . cmpName  . at entityID ?= entity ^. entName

    addScriptComponent  entityID entity
    addPhysicsComponent entityID entity
    addPdPatchComponent entityID entity

    forM_ (entity ^. entChildren) $ \child -> do
        childID <- createEntity persistence child
        wldComponents . cmpParent . at childID ?= entityID
    
    return entityID
