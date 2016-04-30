{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module PreludeExtra 
    ( module Exports
    , module PreludeExtra
    ) where

import Control.Monad.State as Exports hiding (withState, withStateT)
import Control.Monad.Reader as Exports hiding (local)
import Data.Maybe as Exports
import Data.Foldable as Exports
import Control.Arrow as Exports
import Control.Concurrent as Exports
import Control.Concurrent.STM as Exports

import Data.Char as Exports
import Text.Read as Exports (readMaybe)
import Data.Set as Exports (Set)

import System.Random as Exports
import System.Directory as Exports
import System.FilePath as Exports
import System.Environment as Exports

import Data.Dynamic as Exports
import Debug.Trace as Exports
import Data.Fixed as Exports
import Data.Time as Exports
import Data.IORef as Exports
import System.Mem as Exports

import Control.Monad.Catch as Exports
import Control.DeepSeq as Exports

import Data.Yaml as Exports hiding ((.=), String)
import GHC.Generics as Exports (Generic)

import Control.Lens.Extra as Exports hiding (List, (<.>), children)
import Linear.Extra as Exports hiding (trace)
import Graphics.UI.GLFW.Pal as Exports
import Graphics.GL.Pal as Exports hiding (trace, getNow) -- using a faster getNow in Types
import Graphics.VR.Pal as Exports hiding (getNow)


import Data.ECS as Exports hiding (Key)

-- import qualified Data.Map as Map


-- useMapM_ :: (MonadState s m) => Lens' s (Map k v) -> ((k,v) -> m b) -> m ()
-- useMapM_ aLens f = traverseM_ (Map.toList <$> use aLens) f


traverseM :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
traverseM f x = f >>= traverse x

traverseM_ :: (Monad m, Foldable t) => m (t a) -> (a -> m b) -> m ()
traverseM_ f x = f >>= traverse_ x

useTraverseM_ :: (MonadState s m, Foldable t) => Lens' s (t a) -> (a -> m b) -> m ()
useTraverseM_ aLens f = traverseM_ (use aLens) f



exhaustTChan :: TChan a -> STM [a]
exhaustTChan chan = tryReadTChan chan >>= \case
    Just a -> (a:) <$> exhaustTChan chan
    Nothing -> return []