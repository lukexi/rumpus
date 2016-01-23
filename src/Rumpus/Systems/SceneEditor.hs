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
                                                =<< getEntityOverlappingEntityIDs handEntityID
                    
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

removeEntity :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => EntityID -> m ()
removeEntity entityID = do
    removePhysicsComponents entityID
    removeScriptComponent entityID
    removePdPatchComponent entityID
    removeLifetimeComponent entityID

    wldComponents . cmpParent . at entityID .= Nothing
    -- TODO remove this object from any objects claiming it as a parent.
    -- Or delete them too.
    -- (if we don't delete them, have them inherit their position from this object first)

    wldComponents . cmpPose  . at entityID .= Nothing
    wldComponents . cmpSize  . at entityID .= Nothing
    wldComponents . cmpColor . at entityID .= Nothing
    wldComponents . cmpShape . at entityID .= Nothing
    wldComponents . cmpName  . at entityID .= Nothing



