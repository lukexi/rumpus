{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.Selection where
import Data.ECS
import PreludeExtra

data SelectionSystem = SelectionSystem
    { _selSelectedEntityID :: !(Maybe EntityID)
    }
makeLenses ''SelectionSystem
defineSystemKey ''SelectionSystem

initSelectionSystem :: MonadState ECS m => m ()
initSelectionSystem = do
    registerSystem sysSelection (SelectionSystem Nothing)

getSelectedEntityID :: (MonadState ECS m) => m (Maybe EntityID)
getSelectedEntityID = viewSystem sysSelection selSelectedEntityID

setSelectedEntityID :: (MonadState ECS m) => EntityID -> m ()
setSelectedEntityID entityID = modifySystemState sysSelection $
    selSelectedEntityID ?= entityID

clearSelectedEntityID :: (MonadState ECS m) => m ()
clearSelectedEntityID = modifySystemState sysSelection $
    selSelectedEntityID .= Nothing

