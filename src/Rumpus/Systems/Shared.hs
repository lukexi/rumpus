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
defineComponentKeyWithType "PoseScaled"             [t|M44 GLfloat|]
defineComponentKeyWithType "Size"                   [t|V3 GLfloat|]
defineComponentKeyWithType "Color"                  [t|V4 GLfloat|]
defineComponentKeyWithType "Parent"                 [t|EntityID|]
defineComponentKeyWithType "Children"               [t|[EntityID]|]

-- Script System components (shared by Script and CodeEditor systems)
type Start  = EntityMonad ()
type Update = EntityMonad ()

defineComponentKey ''Start
defineComponentKey ''Update

defineComponentKeyWithType "ScriptData" [t|Dynamic|]


initSharedSystem :: (MonadIO m, MonadState ECS m) => m ()
initSharedSystem = do
    registerComponent "Name" myName (savedComponentInterface myName)
    registerComponent "Pose" myPose (savedComponentInterface myPose)
    registerComponent "PoseScaled" myPoseScaled $ (newComponentInterface myPoseScaled) 
        {   ciDeriveComponent = Just $ do
                -- More hax for release; one problem with this is that every entity will now
                -- get a cached scale (even those without shapes, poses or sizes, 
                -- since getSze and getPose return defaults)
                -- but I guess there aren't so many without shapes yet
                size <- getSize
                pose <- getPose
                myPoseScaled ==> pose !*! scaleMatrix size
        }
    registerComponent "Size" mySize (savedComponentInterface mySize)
    registerComponent "Color" myColor (savedComponentInterface myColor)
    registerComponent "ShapeType" myShapeType (savedComponentInterface myShapeType)
    registerComponent "Parent" myParent $ (newComponentInterface myParent)
        { ciDeriveComponent = Just $ do
            withComponent_ myParent $ \parentID -> do
                childID <- ask
                getEntityComponent parentID myChildren >>= \case
                    Nothing -> setEntityComponent myChildren [childID] parentID
                    Just _ ->  modifyEntityComponent parentID myChildren (return . (childID:))
        }
    registerComponent "Children" myChildren $ (newComponentInterface myChildren)
        { ciRemoveComponent = removeChildren
        }
    registerComponent "InheritParentTransform" myInheritParentTransform (newComponentInterface myInheritParentTransform)

    -- Allows Script and CodeEditor to access these
    registerComponent "Start"  myStart      (newComponentInterface myStart)
    registerComponent "Update" myUpdate     (newComponentInterface myUpdate)
    registerComponent "ScriptData" myScriptData (newComponentInterface myScriptData)

removeChildren :: (MonadState ECS m, MonadReader EntityID m, MonadIO m) => m ()
removeChildren = 
    withComponent_ myChildren (mapM_ removeEntity)


setEntityColor :: (MonadState ECS m, MonadIO m) => V4 GLfloat -> EntityID -> m ()
setEntityColor newColor entityID = setEntityComponent myColor newColor entityID

setColor :: (MonadReader EntityID m, MonadState ECS m, MonadIO m) => V4 GLfloat -> m ()
setColor newColor = setComponent myColor newColor


getEntityIDsWithName :: MonadState ECS m => String -> m [EntityID]
getEntityIDsWithName name = fromMaybe [] <$> withComponentMap myName (return . Map.keys . Map.filter (== name))

getEntityName :: MonadState ECS m => EntityID -> m String
getEntityName entityID = fromMaybe "No Name" <$> getEntityComponent entityID myName

getName :: (MonadReader EntityID m, MonadState ECS m) => m String
getName = getEntityName =<< ask

getEntityPose :: MonadState ECS m => EntityID -> m (M44 GLfloat)
getEntityPose entityID = fromMaybe identity <$> getEntityComponent entityID myPose

getPose :: (MonadReader EntityID m, MonadState ECS m) => m (M44 GLfloat)
getPose = getEntityPose =<< ask

getEntitySize :: MonadState ECS m => EntityID -> m (V3 GLfloat)
getEntitySize entityID = fromMaybe 1 <$> getEntityComponent entityID mySize

getSize :: (MonadReader EntityID m, MonadState ECS m) => m (V3 GLfloat)
getSize = getEntitySize =<< ask

getEntityColor :: MonadState ECS m => EntityID -> m (V4 GLfloat)
getEntityColor entityID = fromMaybe 1 <$> getEntityComponent entityID myColor

getColor :: (MonadReader EntityID m, MonadState ECS m) => m (V4 GLfloat)
getColor = getEntityColor =<< ask

getEntityInheritParentTransform :: (HasComponents s, MonadState s m) => EntityID -> m (Maybe InheritParentTransform)
getEntityInheritParentTransform entityID = getEntityComponent entityID myInheritParentTransform

getInheritParentTransform :: (HasComponents s, MonadState s m, MonadReader EntityID m) => m (Maybe InheritParentTransform)
getInheritParentTransform = getEntityInheritParentTransform =<< ask

getEntityChildren :: (HasComponents s, MonadState s m) => EntityID -> m [EntityID]
getEntityChildren entityID = fromMaybe [] <$> getEntityComponent entityID myChildren