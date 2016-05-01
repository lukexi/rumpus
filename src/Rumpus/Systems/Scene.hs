{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Rumpus.Systems.Scene where
import Data.ECS
import PreludeExtra
--import Rumpus.Systems.Sound

data Scene = Scene
    { _scnFolder :: !FilePath
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



loadScene :: (MonadIO m, MonadState ECS m) => String -> m ()
loadScene sceneFolder = do
    putStrLnIO $ "Loading scene: " ++ sceneFolder
    modifySystemState sysScene (scnScene . scnFolder .= sceneFolder)
    --addPdPatchSearchPath sceneFolder
    loadEntities sceneFolder

saveScene :: ECSMonad ()
saveScene = do
    sceneFolder <- viewSystem sysScene (scnScene . scnFolder)
    saveEntities sceneFolder