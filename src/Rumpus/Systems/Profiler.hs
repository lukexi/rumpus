{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Rumpus.Systems.Profiler where
import PreludeExtra hiding ((<|))
import System.Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import System.Metrics.Distribution (Distribution)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>))
import Data.Time
import GHC.Stats

data ProfilerSystem = ProfilerSystem
    { _prfSampleHistory :: !(Map String (Seq Double))
    }
makeLenses ''ProfilerSystem
defineSystemKey ''ProfilerSystem

initProfilerSystem :: (MonadIO m, MonadState ECS m) => m ()
initProfilerSystem = do
    registerSystem sysProfiler (ProfilerSystem mempty)


maxProfilerHistory = 20

addSample name value = do
    modifySystemState sysProfiler $ do
        hist <- use (prfSampleHistory . at name)
        prfSampleHistory . at name ?= case hist of
            Just existing -> Seq.take maxProfilerHistory (value <| existing)
            Nothing       -> Seq.singleton value

getSampleHistory :: MonadState ECS m => m (Map String (Seq Double))
getSampleHistory = viewSystem sysProfiler prfSampleHistory

profile :: String -> ECSMonad a -> ECSMonad a
profile name action = do
    before <- liftIO getCurrentTime
    a      <- action
    after  <- liftIO getCurrentTime

    addSample name (realToFrac $ after `diffUTCTime` before)

    return a
