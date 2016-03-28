{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.Selection where
import Data.ECS
import PreludeExtra
import Rumpus.Systems.Sound

data Scene = Scene
    { _scnFolder :: !FilePath
    }
makeLenses ''Scene

scenesRoot :: FilePath
scenesRoot = "scenes"

newScene :: Scene
newScene = Scene 
    { _scnFolder = scenesRoot </> "NewScene"
    }

data SelectionSystem = SelectionSystem 
    { _selSelectedEntityID :: !(Maybe EntityID) 
    , _selScene            :: !Scene
    }
makeLenses ''SelectionSystem

defineSystemKey ''SelectionSystem

initSelectionSystem :: MonadState ECS m => m ()
initSelectionSystem = do
    registerSystem sysSelection (SelectionSystem Nothing newScene)


loadScene :: (MonadIO m, MonadState ECS m) => String -> m ()
loadScene sceneName = do
    let sceneFolder = scenesRoot </> sceneName
    putStrLnIO $ "Loading scene: " ++ sceneFolder
    modifySystemState sysSelection (selScene . scnFolder .= sceneFolder)
    addPdPatchSearchPath sceneFolder
    loadEntities sceneFolder

saveScene :: ECSMonad ()
saveScene = do
    sceneFolder <- viewSystem sysSelection (selScene . scnFolder)
    saveEntities sceneFolder

getSelectedEntityID :: (MonadState ECS m) => m (Maybe EntityID)
getSelectedEntityID = viewSystem sysSelection selSelectedEntityID

setSelectedEntityID :: (MonadState ECS m) => EntityID -> m ()
setSelectedEntityID entityID = modifySystemState sysSelection $ selSelectedEntityID ?= entityID

clearSelectedEntityID :: (MonadState ECS m) => m ()
clearSelectedEntityID = modifySystemState sysSelection $ selSelectedEntityID .= Nothing