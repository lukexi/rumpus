{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Lifetime where
import PreludeExtra

import Rumpus.Types
import Rumpus.ECS
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics

data Lifetime = Lifetime UTCTime NominalDiffTime
defineComponentKey ''Lifetime

lifetimeSystem :: (MonadIO m, MonadState World m) => m ()
lifetimeSystem = do
    now <- liftIO getCurrentTime
    

    forEntitiesWithComponent lifetimeKey $ \(entityID, Lifetime birthtime lifetime) -> do
        let age = now `diffUTCTime` birthtime
        
        let fadeStart = lifetime - 1
        when (age > fadeStart) $ do
            let fadeProgress = lifetime - age
            size <- getEntitySize entityID
            setEntitySize (size * realToFrac fadeProgress) entityID

        when (age > lifetime) $ 
            removeEntity entityID
        

addLifetimeComponent :: (MonadIO m, MonadState World m) => EntityID -> DiffTime -> m ()
addLifetimeComponent entityID lifetime = do
    birth <- liftIO getCurrentTime
    addComponent lifetimeKey (Lifetime birth (realToFrac lifetime)) entityID




