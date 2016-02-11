{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Lifetime where
import PreludeExtra

import Rumpus.Types
import Data.ECS
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics

data Lifetime = Lifetime UTCTime NominalDiffTime
defineComponentKey ''Lifetime

tickLifetimeSystem :: (MonadIO m, MonadState ECS m) => m ()
tickLifetimeSystem = do
    now <- liftIO getCurrentTime
    

    forEntitiesWithComponent cmpLifetime $ \(entityID, Lifetime birthtime lifetime) -> do
        let age = now `diffUTCTime` birthtime
        
        let fadeStart = lifetime - 1
        when (age > fadeStart) $ do
            let fadeProgress = lifetime - age
            size <- getEntitySize entityID
            setEntitySize (size * realToFrac fadeProgress) entityID

        when (age > lifetime) $ 
            removeEntity entityID
        

addLifetimeComponent :: (MonadIO m, MonadState ECS m) => EntityID -> DiffTime -> m ()
addLifetimeComponent entityID lifetime = do
    birth <- liftIO getCurrentTime
    addComponent cmpLifetime (Lifetime birth (realToFrac lifetime)) entityID




