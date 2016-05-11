{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Rumpus.Systems.Scene where
import Data.ECS
import PreludeExtra

data Scene = Scene
    { _scnFolder  :: !FilePath
    }
makeLenses ''Scene

data SceneSystem = SceneSystem
    { _scnScene            :: !Scene
    }
makeLenses ''SceneSystem

defineSystemKey ''SceneSystem

initSceneSystem :: MonadState ECS m => FilePath -> m ()
initSceneSystem sceneFolder = do
    registerSystem sysScene (SceneSystem (Scene sceneFolder))

getSceneFolder :: (MonadState ECS m) => m FilePath
getSceneFolder = viewSystem sysScene (scnScene . scnFolder)

setSceneFolder :: MonadState ECS m => FilePath -> m ()
setSceneFolder sceneFolder = modifySystemState sysScene (scnScene . scnFolder .= sceneFolder)

loadScene :: (MonadIO m, MonadState ECS m) => String -> m ()
loadScene sceneFolder = do
    putStrLnIO $ "Loading scene: " ++ sceneFolder
    setSceneFolder sceneFolder

    loadEntities (sceneFolder </> ".world-state")

saveScene :: ECSMonad ()
saveScene = do
    sceneFolder <- getSceneFolder
    -- FIXME: move the .world-state concept into extensible-ecs
    liftIO $ removeDirectoryRecursive (sceneFolder </> ".world-state")
    liftIO $ createDirectoryIfMissing True (sceneFolder </> ".world-state")
    saveEntities (sceneFolder </> ".world-state")

fileInScene :: MonadState ECS m => FilePath -> m FilePath
fileInScene fileName = do
    sceneFolder <- getSceneFolder
    return (sceneFolder </> fileName)
