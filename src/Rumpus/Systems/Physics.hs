{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Rumpus.Systems.Physics where
import PreludeExtra
import Data.ECS
import Rumpus.Systems.Shared
import Rumpus.Systems.Script


data PhysicsSystem = PhysicsSystem { _phyDynamicsWorld :: DynamicsWorld } deriving Show
makeLenses ''PhysicsSystem

defineSystemKey ''PhysicsSystem


defineComponentKey ''RigidBody
defineComponentKey ''SpringConstraint


defineComponentKeyWithType "Mass" [t|GLfloat|]

data PhysicsProperty = IsKinematic | NoContactResponse 
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

type PhysicsProperties = [PhysicsProperty]

defineComponentKey ''PhysicsProperties

initPhysicsSystem :: (MonadIO m, MonadState ECS m) => m ()
initPhysicsSystem = do
    dynamicsWorld <- createDynamicsWorld mempty
    registerSystem sysPhysics (PhysicsSystem dynamicsWorld)

    registerComponent "RigidBody" cmpRigidBody $ ComponentInterface 
        { ciAddComponent     = Nothing
        , ciExtractComponent = Nothing
        , ciRestoreComponent = Nothing
        , ciDeriveComponent = Just (deriveRigidBody dynamicsWorld)
        , ciRemoveComponent  = \entityID -> 
                withComponent entityID cmpRigidBody $ \rigidBody -> do
                    removeRigidBody dynamicsWorld rigidBody
                    removeComponent cmpRigidBody entityID
        }
    registerComponent "Mass" cmpMass (defaultComponentInterface cmpMass 1)
    registerComponent "SpringConstraint" cmpSpringConstraint (newComponentInterface cmpSpringConstraint)

deriveRigidBody :: (MonadIO m, MonadState ECS m) => DynamicsWorld -> EntityID -> m ()
deriveRigidBody dynamicsWorld entityID = do
    mShapeType <- getComponent entityID cmpShapeType
    forM_ mShapeType $ \shapeType -> do

        mass <- fromMaybe 1 <$> getComponent entityID cmpMass
        size <- getEntitySize entityID
        pose <- getEntityPose entityID
        physProperties <- fromMaybe [] <$> getComponent entityID cmpPhysicsProperties
        let collisionID = CollisionObjectID entityID
            bodyInfo = mempty { rbPosition = pose ^. posPosition
                              , rbRotation = pose ^. posOrientation
                              , rbMass     = mass
                              }
        shape <- createShapeCollider shapeType size
        rigidBody <- addRigidBody dynamicsWorld collisionID shape bodyInfo
        when (NoContactResponse `elem` physProperties || IsKinematic `elem` physProperties) $ do
            setRigidBodyKinematic rigidBody True

        when (NoContactResponse `elem` physProperties) $ 
            setRigidBodyNoContactResponse rigidBody True
        
        addComponent cmpRigidBody rigidBody entityID

tickPhysicsSystem :: (MonadIO m, MonadState ECS m) => m ()
tickPhysicsSystem = do
    dynamicsWorld <- viewSystem sysPhysics phyDynamicsWorld
    stepSimulation dynamicsWorld 90

-- | Copy poses from Bullet's DynamicsWorld into our own cmpPose components
tickSyncPhysicsPosesSystem :: (MonadIO m, MonadState ECS m) => m ()
tickSyncPhysicsPosesSystem = do
    -- Sync rigid bodies with entity poses
    forEntitiesWithComponent cmpRigidBody $
        \(entityID, rigidBody) -> do
            pose <- uncurry Pose <$> getBodyState rigidBody
            setComponent cmpPose pose entityID

-- | Loop through the collisions for this frame and call any 
-- entities' registered collision callbacks
tickCollisionsSystem :: ECSMonad ()
tickCollisionsSystem = do

    -- NOTE: we get stale collisions with bullet-mini's getCollisions, 
    -- so I've switched to the "contactTest" API which works.

    forEntitiesWithComponent cmpOnCollision $ \(entityID, onCollision) -> do
        collidingIDs <- getEntityOverlappingEntityIDs entityID
        forM_ collidingIDs $ \collidingID -> 
            onCollision entityID collidingID 0.1


createShapeCollider :: (Fractional a, Real a, MonadIO m) => ShapeType -> V3 a -> m CollisionShape
createShapeCollider shapeType size = case shapeType of
    CubeShape        -> createBoxShape         size
    SphereShape      -> createSphereShape      (size ^. _x)
    StaticPlaneShape -> createStaticPlaneShape (0 :: Int)

withEntityRigidBody :: MonadState ECS m => EntityID -> (RigidBody -> m b) -> m ()
withEntityRigidBody entityID = withComponent entityID cmpRigidBody


getEntityOverlapping :: (MonadState ECS m, MonadIO m) => EntityID -> m [Collision]
getEntityOverlapping entityID = getComponent entityID cmpRigidBody  >>= \case
    Nothing          -> return []
    Just rigidBody -> do
        fmap (fromMaybe []) $ 
            withSystem sysPhysics $ \(PhysicsSystem dynamicsWorld) -> 
                contactTest dynamicsWorld rigidBody

getEntityOverlappingEntityIDs :: (MonadState ECS m, MonadIO m) => EntityID -> m [EntityID]
getEntityOverlappingEntityIDs entityID = 
    filter (/= entityID) 
    . concatMap (\c -> [unCollisionObjectID (cbBodyAID c), unCollisionObjectID (cbBodyBID c)]) 
    <$> getEntityOverlapping entityID


setEntitySize :: (MonadIO m, MonadState ECS m) => V3 GLfloat -> EntityID -> m ()
setEntitySize newSize entityID = do

    setComponent cmpSize newSize entityID

    withEntityRigidBody entityID $ \rigidBody -> do 
        withSystem sysPhysics $ \(PhysicsSystem dynamicsWorld) -> do
            mass       <- fromMaybe 1         <$> getComponent entityID cmpMass
            shapeType  <- fromMaybe CubeShape <$> getComponent entityID cmpShapeType

            shape      <- createShapeCollider shapeType newSize
            setRigidBodyShape dynamicsWorld rigidBody shape mass



setEntityPose :: (MonadState ECS m, MonadIO m) => Pose GLfloat -> EntityID -> m ()
setEntityPose newPose_ entityID = do

    setComponent cmpPose newPose_ entityID

    withEntityRigidBody entityID $ \rigidBody -> 
        setRigidBodyWorldTransform rigidBody (newPose_ ^. posPosition) (newPose_ ^. posOrientation)
