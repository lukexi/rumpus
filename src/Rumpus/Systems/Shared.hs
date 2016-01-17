{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Rumpus.Systems.Shared where
import PreludeExtra

import Rumpus.Types
import qualified Data.Map as Map

setEntityColor :: (MonadState World m, MonadReader WorldStatic m) => V4 GLfloat -> EntityID -> m ()
setEntityColor newColor entityID = wldComponents . cmpColor . ix entityID .= newColor


getEntityIDsWithName :: MonadState World m => String -> m [EntityID]
getEntityIDsWithName name = 
    Map.keys . Map.filter (== name) <$> use (wldComponents . cmpName)

getEntityName :: MonadState World m => EntityID -> m String
getEntityName entityID = fromMaybe "No Name" <$> use (wldComponents . cmpName . at entityID)

getEntityPose :: MonadState World m => EntityID -> m (Pose GLfloat)
getEntityPose entityID = fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)

getEntitySize :: MonadState World m => EntityID -> m (V3 GLfloat)
getEntitySize entityID = fromMaybe 1 <$> use (wldComponents . cmpSize . at entityID)

getEntityColor :: MonadState World m => EntityID -> m (V4 GLfloat)
getEntityColor entityID = fromMaybe 1 <$> use (wldComponents . cmpColor . at entityID)

traverseM :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
traverseM f x = f >>= traverse x

traverseM_ :: (Monad m, Foldable t) => m (t a) -> (a -> m b) -> m ()
traverseM_ f x = f >>= traverse_ x

useMaybeM_ :: (MonadState s m) => Lens' s (Maybe a) -> (a -> m b) -> m ()
useMaybeM_ aLens f = do
    current <- use aLens
    mapM_ f current
