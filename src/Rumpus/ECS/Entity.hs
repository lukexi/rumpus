{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.ECS.Entity where
import Control.Lens.Extra
import Control.Monad.State
import qualified Data.Map as Map
import System.Random
import Data.List
import Rumpus.Types


newEntity :: MonadIO m => m EntityID
newEntity = liftIO randomIO


createEntity :: (MonadState World m, MonadIO m) => m EntityID
createEntity = do
    entityID <- newEntity
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> ciAddComponent entityID)
    wldEntities %= (entityID:)

    return entityID


removeEntity :: (MonadState World m, MonadIO m) => EntityID -> m ()
removeEntity entityID = do
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> ciRemoveComponent entityID)
    wldEntities %= delete entityID


saveEntities :: (MonadState World m, MonadIO m) => m ()
saveEntities = do
    entities <- use wldEntities
    componentInterfaces <- Map.toList <$> use wldComponentLibrary
    forM_ entities $ \entityID -> do
        yaml <- foldM (\entityMap (componentName, ComponentInterface{..}) -> do
            mValue <- join <$> forM ciExtractComponent ($ entityID)
            return $ case mValue of
                Just value -> Map.insert componentName value entityMap
                Nothing -> entityMap
            ) mempty componentInterfaces
        liftIO $ print (entityID, yaml)
