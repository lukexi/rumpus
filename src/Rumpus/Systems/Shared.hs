{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Rumpus.Systems.Shared where
import PreludeExtra

import Rumpus.ECS
import Rumpus.Types
import qualified Data.Map as Map

data ShapeType = CubeShape | SphereShape | StaticPlaneShape 
    deriving (Eq, Show, Ord, Enum, Generic, FromJSON, ToJSON)
defineComponentKey ''ShapeType


defineComponentKeyWithType "Name" [t|String|]

defineComponentKeyWithType "Pose" [t|Pose GLfloat|]

defineComponentKeyWithType "Size" [t|V3 GLfloat|]

defineComponentKeyWithType "Color" [t|V4 GLfloat|]

defineComponentKeyWithType "Parent" [t|EntityID|]

{-
FIXME: in registerComponent Parent, must remove children!
traverseM_ (Map.toList <$> use (wldComponents . cmpParent)) $ \(childID, childParentID) -> do
        when (childParentID == entityID) $ 
            removeEntity childID
-}

setEntityColor :: (MonadState World m, MonadIO m) => V4 GLfloat -> EntityID -> m ()
setEntityColor newColor entityID = setComponent cmpColor newColor entityID


getEntityIDsWithName :: MonadState World m => String -> m [EntityID]
getEntityIDsWithName name = fromMaybe [] <$> withComponentMap cmpName (return . Map.keys . Map.filter (== name))

getEntityName :: MonadState World m => EntityID -> m String
getEntityName entityID = fromMaybe "No Name" <$> getComponent entityID cmpName

getEntityPose :: MonadState World m => EntityID -> m (Pose GLfloat)
getEntityPose entityID = fromMaybe newPose <$> getComponent entityID cmpPose

getEntitySize :: MonadState World m => EntityID -> m (V3 GLfloat)
getEntitySize entityID = fromMaybe 1 <$> getComponent entityID cmpSize

getEntityColor :: MonadState World m => EntityID -> m (V4 GLfloat)
getEntityColor entityID = fromMaybe 1 <$> getComponent entityID cmpColor

