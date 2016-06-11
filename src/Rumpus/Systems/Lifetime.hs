module Rumpus.Systems.Lifetime where
import PreludeExtra

import Rumpus.Systems.Shared
import Rumpus.Systems.Physics

data ActiveLifetime = ActiveLifetime UTCTime NominalDiffTime
defineComponentKey ''ActiveLifetime
defineComponentKeyWithType "Lifetime" [t|DiffTime|]

initLifetimeSystem :: MonadState ECS m => m ()
initLifetimeSystem = do
    registerComponent "ActiveLifetime" myActiveLifetime (newComponentInterface myActiveLifetime)
    registerComponent "Lifetime" myLifetime $ (newComponentInterface myLifetime)
        { ciDeriveComponent = Just deriveActiveLifetime
        }

deriveActiveLifetime :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => m ()
deriveActiveLifetime = do
    withComponent_ myLifetime $ \lifetime -> do
        birth <- liftIO getCurrentTime
        myActiveLifetime ==> ActiveLifetime birth (realToFrac lifetime)

tickLifetimeSystem :: (MonadIO m, MonadState ECS m) => m ()
tickLifetimeSystem = do
    now <- liftIO getCurrentTime

    forEntitiesWithComponent myActiveLifetime $ \(entityID, ActiveLifetime birthtime lifetime) -> do
        let age       = now `diffUTCTime` birthtime
            fadeStart = lifetime - 1

        when (age > fadeStart) $ do
            let fadeProgress = lifetime - age
            size <- getEntitySize entityID
            setEntitySize entityID (size * realToFrac fadeProgress)

        when (age > lifetime) $ do
            removeEntity entityID


setLifetime :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => DiffTime -> m ()
setLifetime lifetime = do
    myLifetime ==> lifetime
    deriveActiveLifetime




