{-# LANGUAGE DeriveAnyClass #-}
module Rumpus.Systems.Shared where
import PreludeExtra
import qualified Data.HashMap.Strict as Map
import qualified Data.List as L

data ShapeType = Cube | Sphere
    deriving (Eq, Show, Ord, Enum, Generic, FromJSON, ToJSON)

data TransformType = AbsolutePose  -- ^ No inheritance at all
                   | RelativePose  -- ^ Default, relative to parent
                   | RelativeFull  -- ^ Inherit position and scale from parent.
                                  --   Only uniform scaling will look right when rotations are involved.
                   deriving (Eq, Show)

defineComponentKey ''TransformType
defineComponentKeyWithType "Shape"              [t|ShapeType|]
defineComponentKeyWithType "Name"               [t|String|]
defineComponentKeyWithType "Pose"               [t|M44 GLfloat|]
defineComponentKeyWithType "PoseScaled"         [t|M44 GLfloat|]
defineComponentKeyWithType "Size"               [t|V3 GLfloat|]
defineComponentKeyWithType "Color"              [t|V4 GLfloat|]
defineComponentKeyWithType "Parent"             [t|EntityID|]
defineComponentKeyWithType "Children"           [t|[EntityID]|]
defineComponentKeyWithType "TeleportScale"      [t|GLfloat|]

defineComponentKeyWithType "ColorAnimation"     [t|Animation (V4 GLfloat)|]
defineComponentKeyWithType "SizeAnimation"      [t|Animation (V3 GLfloat)|]
defineComponentKeyWithType "PositionAnimation"  [t|Animation (V3 GLfloat)|]
defineComponentKeyWithType "RotationAnimation"  [t|Animation (Quaternion GLfloat)|]

-- Script System components (shared by Script and CodeEditor systems)
type Start  = EntityMonad ()
type Update = EntityMonad ()

defineComponentKey ''Start
defineComponentKey ''Update

defineComponentKeyWithType "State" [t|Dynamic|]

-- | Pairs a filename along with an expression
-- to evaluate in that filename's environment once compiled
type CodeFile      = (FilePath, String)
type SceneCodeFile = (FilePath, FilePath, String)
-- FIXME: these should move to their companion definitions (i.e. myStart, myUpdate etc.)
-- and those files should depend on CodeEditor and call registerCodeExprComponent.
-- Must thus be initialized after CodeEditor.
defineComponentKeyWithType "StartCodeFile"  [t|CodeFile|]
defineComponentKeyWithType "StartSceneCodeFile"  [t|SceneCodeFile|]
defineComponentKeyWithType "UpdateCodeFile" [t|CodeFile|]
defineComponentKeyWithType "CodeHidden"     [t|Bool|]

initSharedSystem :: (MonadIO m, MonadState ECS m) => m ()
initSharedSystem = do
    registerComponent "Name" myName (savedComponentInterface myName)
    registerComponent "Pose" myPose (savedComponentInterface myPose)
    registerComponent "PoseScaled" myPoseScaled $ (newComponentInterface myPoseScaled)
        {   ciDeriveComponent = Just $ do
                -- More hax for release; one problem with this is that every entity will now
                -- get a cached scale (even those without shapes, poses or sizes,
                -- since getSize and getPose return defaults)
                -- but I guess there aren't so many without shapes yet
                size <- getSize
                pose <- getPose
                myPoseScaled ==> pose !*! scaleMatrix size
        }
    registerComponent "Size" mySize (savedComponentInterface mySize)
    registerComponent "Color" myColor (savedComponentInterface myColor)
    registerComponent "Shape" myShape (savedComponentInterface myShape)
    registerComponent "Parent" myParent $ (newComponentInterface myParent)
        { ciDeriveComponent = Just $ do
            withComponent_ myParent setParent
        , ciRemoveComponent = removeFromParent >> removeComponent myParent
        }
    registerComponent "Children" myChildren $ (newComponentInterface myChildren)
        { ciRemoveComponent = removeChildren >> removeComponent myChildren
        }
    registerComponent "TransformType" myTransformType (newComponentInterface myTransformType)
    registerComponent "TeleportScale" myTeleportScale (newComponentInterface myTeleportScale)

    -- Allows Script and CodeEditor to access these
    registerComponent "Start"  myStart      (newComponentInterface myStart)
    registerComponent "Update" myUpdate     (newComponentInterface myUpdate)
    registerComponent "State"  myState      (newComponentInterface myState)

setParent :: (MonadState ECS m, MonadReader EntityID m) => EntityID -> m ()
setParent newParentID = do

    removeFromParent

    myParent ==> newParentID

    childID <- ask
    inEntity newParentID $ do
        getComponent myChildren >>= \case
            Nothing -> myChildren ==> [childID]
            Just _ ->  myChildren ==% (childID:)

getParent :: (MonadState ECS m, MonadReader EntityID m) => m (Maybe EntityID)
getParent = getComponent myParent

removeFromParent :: (MonadReader EntityID m, MonadState ECS m) => m ()
removeFromParent = do
    childID <- ask
    withComponent_ myParent $ \oldParentID ->
        inEntity oldParentID $ do
            myChildren ==% L.delete childID
    removeComponent myParent

removeChildren :: (MonadState ECS m, MonadReader EntityID m, MonadIO m) => m ()
removeChildren =
    withComponent_ myChildren (mapM_ removeEntity)

spawnChild_ :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m, MonadReader EntityID m) => ReaderT EntityID m () -> m ()
spawnChild_ = void . spawnChild

spawnChildren :: (Traversable t, MonadIO m, MonadState ECS m,
                  MonadReader EntityID m, MonadBaseControl IO m)
              => t a -> (a -> ReaderT EntityID m ()) -> m (t EntityID)
spawnChildren xs action = forM xs (spawnChild . action)

spawnChildren_ :: (Traversable t, MonadIO m, MonadState ECS m,
                   MonadReader EntityID m, MonadBaseControl IO m)
               => t a -> (a -> ReaderT EntityID m ()) -> m ()
spawnChildren_ xs = void . spawnChildren xs

spawnChild :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m, MonadReader EntityID m) => ReaderT EntityID m () -> m EntityID
spawnChild actions = do
    thisEntityID <- ask
    spawnChildOf thisEntityID actions

spawnChildOf_ :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m) => EntityID -> ReaderT EntityID m () -> m ()
spawnChildOf_ entityID = void . spawnChildOf entityID

spawnChildOf :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m) => EntityID -> ReaderT EntityID m () -> m EntityID
spawnChildOf entityID actions =
    spawnEntity $ do
        myParent ==> entityID
        actions

setEntityColor :: (MonadState ECS m) => EntityID -> V4 GLfloat -> m ()
setEntityColor entityID newColor = setEntityComponent myColor newColor entityID

setColor :: (MonadReader EntityID m, MonadState ECS m) => V4 GLfloat -> m ()
setColor newColor = setComponent myColor newColor


getEntityIDsWithName :: MonadState ECS m => String -> m [EntityID]
getEntityIDsWithName name = fromMaybe [] <$> withComponentMap myName (return . Map.keys . Map.filter (== name))

getEntityName :: MonadState ECS m => EntityID -> m String
getEntityName entityID = fromMaybe "No Name" <$> getEntityComponent entityID myName

getName :: (MonadReader EntityID m, MonadState ECS m) => m String
getName = getEntityName =<< ask

getEntityPose :: MonadState ECS m => EntityID -> m (M44 GLfloat)
getEntityPose entityID = fromMaybe identity <$> getEntityComponent entityID myPose

getEntityPosition :: MonadState ECS m => EntityID -> m (V3 GLfloat)
getEntityPosition entityID = view translation <$> getEntityPose entityID

getPosition :: (MonadReader EntityID m, MonadState ECS m) => m (V3 GLfloat)
getPosition = getEntityPosition =<< ask

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

getEntityTransformType :: (MonadState ECS m) => EntityID -> m (Maybe TransformType)
getEntityTransformType entityID = getEntityComponent entityID myTransformType

getTransformType :: (MonadState ECS m, MonadReader EntityID m) => m (Maybe TransformType)
getTransformType = getEntityTransformType =<< ask

getEntityChildren :: (MonadState ECS m) => EntityID -> m [EntityID]
getEntityChildren entityID = fromMaybe [] <$> getEntityComponent entityID myChildren

getEntityTeleportScale :: (MonadState ECS m) => EntityID -> m GLfloat
getEntityTeleportScale entityID = fromMaybe 1 <$> getEntityComponent entityID myTeleportScale

getTeleportScale :: (MonadReader EntityID m, MonadState ECS m) => m GLfloat
getTeleportScale = getEntityTeleportScale =<< ask
