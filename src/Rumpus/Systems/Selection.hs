{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.Selection where
import Data.ECS
import PreludeExtra

data SelectionSystem = SelectionSystem { _selSelectedEntityID   :: !(Maybe EntityID) }
makeLenses ''SelectionSystem

defineSystemKey ''SelectionSystem

initSelectionSystem :: MonadState ECS m => m ()
initSelectionSystem = do
    registerSystem sysSelection (SelectionSystem Nothing)
