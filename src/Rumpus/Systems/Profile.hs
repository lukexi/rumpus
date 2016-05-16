{-# LANGUAGE OverloadedStrings #-}

module Rumpus.Systems.Profiler where

import System.Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>))

data ProfilerSystem = ProfilerSystem
    { _prfStore         :: Store
    , _prfDistributions :: Map Text Distribution
    , _prfSampleHistory :: Seq Sample
    }
makeLenses ''ProfilerSystem
defineSystemKey ''ProfilerSystem

initProfilerSystem :: MonadState ECS m => m ()
initProfilerSystem = do

    store <- newStore
    registerGcMetrics store

    registerSystem sysProfiler (ProfilerSystem store mempty mempty)

profile :: Text -> ECSMonad () -> ECSMonad ()
profile name action = do
    distributions <- sysProfiler `viewSystem` prfDistributions
    timeDistribution <- case Map.lookup name distributions of
        Just distribution -> return distribution
        Nothing -> do
            store <- viewSystem sysProfiler prfStore
            liftIO (createDistribution name store)

    liftIO $ Distribution.add time timeDistribution


