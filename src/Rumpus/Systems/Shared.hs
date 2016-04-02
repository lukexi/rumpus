{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Rumpus.Systems.Shared where
import PreludeExtra
import qualified Data.HashMap.Strict as Map

data ShapeType = CubeShape | SphereShape | StaticPlaneShape 
    deriving (Eq, Show, Ord, Enum, Generic, FromJSON, ToJSON)

data InheritParentTransform = InheritFull | InheritPose

defineComponentKey ''ShapeType
defineComponentKey ''InheritParentTransform
defineComponentKeyWithType "Name"                   [t|String|]
defineComponentKeyWithType "Pose"                   [t|M44 GLfloat|]
defineComponentKeyWithType "Size"                   [t|V3 GLfloat|]
defineComponentKeyWithType "Color"                  [t|V4 GLfloat|]
defineComponentKeyWithType "Parent"                 [t|EntityID|]
defineComponentKeyWithType "Children"               [t|[EntityID]|]

-- Script System components (shared by Script and CodeEditor systems)
type OnStart  = EntityMonad (Maybe Dynamic)
type OnUpdate = EntityMonad ()

defineComponentKey ''OnStart
defineComponentKey ''OnUpdate

defineComponentKeyWithType "ScriptData" [t|Dynamic|]


initSharedSystem :: (MonadIO m, MonadState ECS m) => m ()
initSharedSystem = do
    registerComponent "Name" cmpName (savedComponentInterface cmpName)
    registerComponent "Pose" cmpPose (savedComponentInterface cmpPose)
    registerComponent "Size" cmpSize (savedComponentInterface cmpSize)
    registerComponent "Color" cmpColor (savedComponentInterface cmpColor)
    registerComponent "ShapeType" cmpShapeType (savedComponentInterface cmpShapeType)
    registerComponent "Parent" cmpParent $ (newComponentInterface cmpParent)
        { ciDeriveComponent = Just $ do
            withComponent_ cmpParent $ \parentID -> do
                childID <- ask
                getEntityComponent parentID cmpChildren >>= \case
                    Nothing -> setEntityComponent cmpChildren [childID] parentID
                    Just _ ->  modifyEntityComponent parentID cmpChildren (return . (childID:))
        }
    registerComponent "Children" cmpChildren $ (newComponentInterface cmpChildren)
        { ciRemoveComponent = removeChildren
        }
    registerComponent "InheritParentTransform" cmpInheritParentTransform (newComponentInterface cmpInheritParentTransform)

    -- Allows Script and CodeEditor to access these
    registerComponent "OnStart"  cmpOnStart      (newComponentInterface cmpOnStart)
    registerComponent "OnUpdate" cmpOnUpdate     (newComponentInterface cmpOnUpdate)
    registerComponent "ScriptData" cmpScriptData (newComponentInterface cmpScriptData)

removeChildren :: (MonadState ECS m, MonadReader EntityID m, MonadIO m) => m ()
removeChildren = 
    withComponent_ cmpChildren (mapM_ removeEntity)


setEntityColor :: (MonadState ECS m, MonadIO m) => V4 GLfloat -> EntityID -> m ()
setEntityColor newColor entityID = setEntityComponent cmpColor newColor entityID

setColor :: (MonadReader EntityID m, MonadState ECS m, MonadIO m) => V4 GLfloat -> m ()
setColor newColor = setComponent cmpColor newColor


getEntityIDsWithName :: MonadState ECS m => String -> m [EntityID]
getEntityIDsWithName name = fromMaybe [] <$> withComponentMap cmpName (return . Map.keys . Map.filter (== name))

getEntityName :: MonadState ECS m => EntityID -> m String
getEntityName entityID = fromMaybe "No Name" <$> getEntityComponent entityID cmpName

getName :: (MonadReader EntityID m, MonadState ECS m) => m String
getName = getEntityName =<< ask

getEntityPose :: MonadState ECS m => EntityID -> m (M44 GLfloat)
getEntityPose entityID = fromMaybe identity <$> getEntityComponent entityID cmpPose

getPose :: (MonadReader EntityID m, MonadState ECS m) => m (M44 GLfloat)
getPose = getEntityPose =<< ask

getEntitySize :: MonadState ECS m => EntityID -> m (V3 GLfloat)
getEntitySize entityID = fromMaybe 1 <$> getEntityComponent entityID cmpSize

getSize :: (MonadReader EntityID m, MonadState ECS m) => m (V3 GLfloat)
getSize = getEntitySize =<< ask

getEntityColor :: MonadState ECS m => EntityID -> m (V4 GLfloat)
getEntityColor entityID = fromMaybe 1 <$> getEntityComponent entityID cmpColor

getColor :: (MonadReader EntityID m, MonadState ECS m) => m (V4 GLfloat)
getColor = getEntityColor =<< ask

getEntityInheritParentTransform :: (HasComponents s, MonadState s m) => EntityID -> m (Maybe InheritParentTransform)
getEntityInheritParentTransform entityID = getEntityComponent entityID cmpInheritParentTransform

getInheritParentTransform :: (HasComponents s, MonadState s m, MonadReader EntityID m) => m (Maybe InheritParentTransform)
getInheritParentTransform = getEntityInheritParentTransform =<< ask

getEntityChildren :: (HasComponents s, MonadState s m) => EntityID -> m [EntityID]
getEntityChildren entityID = fromMaybe [] <$> getEntityComponent entityID cmpChildren