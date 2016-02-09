{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Rumpus.ECS.Component where
import qualified Data.Vault.Strict as Vault
import Data.Vault.Strict (Key)
import Control.Lens.Extra
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import Data.Yaml hiding ((.=))

import Rumpus.Types

registerComponent :: MonadState World m => String -> Key (EntityMap a) -> ComponentInterface -> m ()
registerComponent name componentKey componentInterface = do
    wldComponents %= Vault.insert componentKey mempty
    wldComponentLibrary . at name ?= componentInterface

registerComponentSimple :: (MonadState World m, ToJSON a) => String -> Key (EntityMap a) -> a -> m ()
registerComponentSimple name componentKey initialValue = 
    registerComponent name componentKey $ ComponentInterface 
        { ciAddComponent     = addComponent componentKey initialValue
        , ciRemoveComponent  = removeComponentFromEntity componentKey
        , ciExtractComponent = Just (getComponentJSON componentKey)
        }

withComponentMap_ :: MonadState World m => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m ()
withComponentMap_ componentKey = void . withComponentMap componentKey

withComponentMap :: MonadState World m => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m (Maybe b)
withComponentMap componentKey action = do
    componentMaps <- use wldComponents
    forM (Vault.lookup componentKey componentMaps) action

getComponentMap :: MonadState World m => Key r -> m r
getComponentMap componentKey = do
    componentMaps <- use wldComponents
    return $ fromMaybe getComponentMapError $ Vault.lookup componentKey componentMaps
    where getComponentMapError = error "getComponentMap couldn't find a componentMap for the given key"

-- | Perform an action on each entityID/component pair
forEntitiesWithComponent :: MonadState World m => Key (EntityMap a) -> ((EntityID, a) -> m b) -> m ()
forEntitiesWithComponent componentKey action = 
    withComponentMap_ componentKey $ \componentMap -> 
        forM_ (Map.toList componentMap) action

addComponent :: (MonadIO m, MonadState World m) => Key (EntityMap a) -> a -> EntityID -> m ()
addComponent componentKey value entityID = 
    wldComponents %= Vault.adjust (Map.insert entityID value) componentKey

setComponent :: (MonadIO m, MonadState World m) => Key (EntityMap a) -> a -> EntityID -> m ()
setComponent = addComponent

removeComponentFromEntity :: (MonadState World m) => Key (EntityMap a) -> EntityID -> m ()
removeComponentFromEntity componentKey entityID = 
    wldComponents %= Vault.adjust (Map.delete entityID) componentKey

withComponent :: MonadState World m => EntityID -> Key (EntityMap a) -> (a -> m b) -> m ()
withComponent entityID componentKey action = do
    maybeComponent <- getComponent entityID componentKey
    forM_ maybeComponent action

modifyComponent :: (MonadIO m, MonadState World m) => EntityID -> Key (EntityMap a) -> (a -> m a) -> m ()
modifyComponent entityID componentKey action = do
    maybeComponent <- getComponent entityID componentKey
    mapM action maybeComponent >>= \case
        Just newValue -> addComponent componentKey newValue entityID
        Nothing -> return ()

getComponent :: MonadState World f => EntityID -> Key (EntityMap a) -> f (Maybe a)
getComponent entityID componentKey = 
    fmap join $ withComponentMap componentKey $ \componentMap ->
        return $ Map.lookup entityID componentMap

getComponentJSON :: (MonadState World f, ToJSON a) => Key (EntityMap a) -> EntityID -> f (Maybe Value)
getComponentJSON componentKey entityID = fmap toJSON <$> getComponent entityID componentKey
