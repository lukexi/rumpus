{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Rumpus.ECS.System where
import qualified Data.Vault.Strict as Vault
import Data.Vault.Strict (Key)
import Control.Lens.Extra
import Control.Monad.State
import Data.Maybe

import Rumpus.Types

registerSystem :: MonadState World m => Key a -> a -> m ()
registerSystem = setSystem

setSystem :: MonadState World m => Key a -> a -> m ()
setSystem systemKey system = wldSystems %= Vault.insert systemKey system

withSystem :: MonadState World m => Key a -> (a -> m b) -> m (Maybe b)
withSystem systemKey action = do
    systems <- use wldSystems
    forM (Vault.lookup systemKey systems) action

viewSystem :: MonadState World m => Key s -> Lens' s a -> m a
viewSystem systemKey viewLens = view viewLens <$> getSystem systemKey

getSystem :: MonadState World m => Key b -> m b
getSystem systemKey = do
    systems <- use wldSystems
    return (fromMaybe missingSystem (Vault.lookup systemKey systems))
    where missingSystem = error "Error: getSystem couldn't find a system for the given key!"

withSystem_ :: MonadState World m => Key a -> (a -> m b) -> m ()
withSystem_ systemKey = void . withSystem systemKey

modifySystem :: MonadState World m => Key s -> (s -> m (s, b)) -> m b
modifySystem systemKey action = do
    system <- getSystem systemKey
    (newSystem, b) <- action system
    setSystem systemKey newSystem
    return b

modifySystem_ :: MonadState World m => Key a -> (a -> m a) -> m ()
modifySystem_ systemKey action = void $ modifySystem systemKey (fmap (,()) . action)

modifySystemState :: MonadState World m => Key s -> StateT s m b -> m b
modifySystemState systemKey action = modifySystem systemKey $ \system -> do
    (r, newState) <- runStateT action system
    return (newState, r)
