{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.Selection where
import Data.ECS
import PreludeExtra


data Scene = Scene
    { _scnFolder :: !FilePath
    }
makeLenses ''Scene

data SelectionSystem = SelectionSystem 
    { _selSelectedEntityID :: !(Maybe EntityID) 
    , _selScene            :: !Scene
    }
makeLenses ''SelectionSystem

defineSystemKey ''SelectionSystem

initSelectionSystem :: MonadState ECS m => FilePath -> m ()
initSelectionSystem sceneFolder = do
    registerSystem sysSelection (SelectionSystem Nothing (Scene sceneFolder))

getSelectedEntityID :: (MonadState ECS m) => m (Maybe EntityID)
getSelectedEntityID = viewSystem sysSelection selSelectedEntityID

setSelectedEntityID :: (MonadState ECS m) => EntityID -> m ()
setSelectedEntityID entityID = modifySystemState sysSelection $ selSelectedEntityID ?= entityID

clearSelectedEntityID :: (MonadState ECS m) => m ()
clearSelectedEntityID = modifySystemState sysSelection $ selSelectedEntityID .= Nothing

getSceneFolder :: (MonadState ECS m) => m FilePath
getSceneFolder = viewSystem sysSelection (selScene . scnFolder)
