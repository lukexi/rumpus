{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Lifetime where
import PreludeExtra

import qualified Data.Map as Map

import Rumpus.Types
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Script
import Rumpus.Systems.Sound


lifetimeSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => m ()
lifetimeSystem = do
    now <- liftIO getCurrentTime
    lifetimes <- Map.toList <$> use (wldComponents . cmpLifetime)
    forM_ lifetimes $ \(entityID, Lifetime birthtime lifetime) -> do
        let age = now `diffUTCTime` birthtime
        
        let fadeStart = lifetime - 1
        when (age > fadeStart) $ do
            let fadeProgress = lifetime - age
            size <- getEntitySize entityID
            setEntitySize (size * realToFrac fadeProgress) entityID

        when (age > lifetime) $ 
            removeEntity entityID
        

addLifetimeComponent :: (MonadIO m, MonadState World m) => EntityID -> Entity -> m ()
addLifetimeComponent entityID entity = do

    forM_ (entity ^. entLifetime) $ \lifetime -> do
        birth <- liftIO getCurrentTime
        wldComponents . cmpLifetime . at entityID ?= Lifetime birth (realToFrac lifetime)

removeLifetimeComponent :: MonadState World m => EntityID -> m ()
removeLifetimeComponent entityID = do
    wldComponents . cmpLifetime . at entityID .= Nothing



removeEntity :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => EntityID -> m ()
removeEntity entityID = do
    removePhysicsComponents entityID
    removeScriptComponent entityID
    removePdPatchComponent entityID
    removeLifetimeComponent entityID

    wldComponents . cmpParent . at entityID .= Nothing
    -- TODO remove this object from any objects claiming it as a parent.
    -- Or delete them too.
    -- (if we don't delete them, have them inherit their position from this object first)

    wldComponents . cmpPose  . at entityID .= Nothing
    wldComponents . cmpSize  . at entityID .= Nothing
    wldComponents . cmpColor . at entityID .= Nothing
    wldComponents . cmpScale . at entityID .= Nothing
    wldComponents . cmpShape . at entityID .= Nothing
    wldComponents . cmpName  . at entityID .= Nothing
