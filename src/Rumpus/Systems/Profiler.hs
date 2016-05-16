{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Rumpus.Systems.Profiler where
import PreludeExtra
import System.Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import System.Metrics.Distribution (Distribution)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>))
import Data.Text
import Data.Time

data ProfilerSystem = ProfilerSystem
    { _prfStore         :: Store
    , _prfDistributions :: Map Text Distribution
    , _prfSampleHistory :: Seq Sample
    }
makeLenses ''ProfilerSystem
defineSystemKey ''ProfilerSystem

initProfilerSystem :: (MonadIO m, MonadState ECS m) => m ()
initProfilerSystem = do

    store <- liftIO $ newStore
    liftIO $ registerGcMetrics store

    registerSystem sysProfiler (ProfilerSystem store mempty mempty)

getProfilerStore :: MonadState ECS m => m Store
getProfilerStore = sysProfiler `viewSystem` prfStore

getDistributionNamed name = do
    distributions <- sysProfiler `viewSystem` prfDistributions
    case Map.lookup name distributions of
        Just distribution -> return distribution
        Nothing -> do
            store <- getProfilerStore
            distribution <- liftIO (createDistribution name store)
            modifySystemState sysProfiler $ prfDistributions . at name ?= distribution
            return distribution

profile :: Text -> ECSMonad a -> ECSMonad a
profile name action = do
    before <- liftIO getCurrentTime
    a <- action
    after  <- liftIO getCurrentTime

    timeDistribution <- getDistributionNamed name
    liftIO $ Distribution.add timeDistribution (realToFrac $ after `diffUTCTime` before)

    return a

getProfilerSample :: (MonadState ECS m, MonadIO m) => m Sample
getProfilerSample = liftIO . sampleAll =<< getProfilerStore

logProfile :: (MonadState ECS m, MonadIO m) => m ()
logProfile = printIO =<< getProfilerSample
