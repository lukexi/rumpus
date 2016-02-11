{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.Selection where
import Rumpus.Types
import Data.ECS
import PreludeExtra

data SelectionSystem = SelectionSystem { _selSelectedEntityID   :: !(Maybe EntityID) }
makeLenses ''SelectionSystem

defineSystemKey ''SelectionSystem
