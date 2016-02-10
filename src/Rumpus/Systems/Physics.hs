{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Rumpus.Systems.Physics where
import PreludeExtra
import Rumpus.Types
import Rumpus.ECS
import Rumpus.Systems.Shared
import Rumpus.Systems.Script


data PhysicsSystem = PhysicsSystem { _psDynamicsWorld :: DynamicsWorld } deriving Show
makeLenses ''PhysicsSystem

defineSystemKey ''PhysicsSystem


defineComponentKey ''RigidBody
defineComponentKey ''SpringConstraint


defineComponentKeyWithType "Mass" [t|GLfloat|]

data PhysicsProperty = IsKinematic | NoContactResponse 
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

type PhysicsProperties = [PhysicsProperty]

defineComponentKey ''PhysicsProperties



createPhysicsSystem :: IO DynamicsWorld
createPhysicsSystem = createDynamicsWorld mempty


tickPhysicsSystem :: (MonadIO m, MonadState World m) => m ()
tickPhysicsSystem = do
    dynamicsWorld <- viewSystem physicsSystemKey psDynamicsWorld
    stepSimulation dynamicsWorld 90

-- | Copy poses from Bullet's DynamicsWorld into our own cmpPose components
tickSyncPhysicsPosesSystem :: (MonadIO m, MonadState World m) => m ()
tickSyncPhysicsPosesSystem = do
    -- Sync rigid bodies with entity poses
    forEntitiesWithComponent rigidBodyKey $
        \(entityID, rigidBody) -> do
            pose <- uncurry Pose <$> getBodyState rigidBody
            setComponent poseKey pose entityID

-- | Loop through the collisions for this frame and call any 
-- entities' registered collision callbacks
tickCollisionsSystem :: WorldMonad ()
tickCollisionsSystem = do

    -- NOTE: we get stale collisions with bullet-mini's getCollisions, 
    -- so I've switched to the "contactTest" API which works.

    forEntitiesWithComponent onCollisionKey $ \(entityID, onCollision) -> do
        collidingIDs <- getEntityOverlappingEntityIDs entityID
        forM_ collidingIDs $ \collidingID -> 
            onCollision entityID collidingID 0.1


addPhysicsComponent :: (MonadIO m, MonadState World m) 
                    => EntityID -> GLfloat -> PhysicsProperties -> m ()
addPhysicsComponent entityID mass physProperties = withSystem_ physicsSystemKey $ \(PhysicsSystem dynamicsWorld) -> do

    pose      <- fromMaybe newPose   <$> getComponent entityID poseKey
    size      <- fromMaybe 1         <$> getComponent entityID sizeKey
    shapeType <- fromMaybe CubeShape <$> getComponent entityID shapeTypeKey

    shape <- createShapeCollider shapeType size
            
    let collisionID = CollisionObjectID entityID
        bodyInfo = mempty { rbPosition = pose ^. posPosition
                          , rbRotation = pose ^. posOrientation
                          , rbMass     = mass
                          }
    
    rigidBody <- addRigidBody dynamicsWorld collisionID shape bodyInfo
    
    addComponent rigidBodyKey rigidBody entityID
    addComponent physicsPropertiesKey physProperties entityID

    when (NoContactResponse `elem` physProperties || IsKinematic `elem` physProperties) $ do
        setRigidBodyKinematic rigidBody True

    when (NoContactResponse `elem` physProperties) $ 
        setRigidBodyNoContactResponse rigidBody True

createShapeCollider :: (Fractional a, Real a, MonadIO m) => ShapeType -> V3 a -> m CollisionShape
createShapeCollider shapeType size = case shapeType of
        CubeShape        -> createBoxShape         size
        SphereShape      -> createSphereShape      (size ^. _x)
        StaticPlaneShape -> createStaticPlaneShape (0 :: Int)

removePhysicsComponents :: (MonadIO m, MonadState World m) => EntityID -> m ()
removePhysicsComponents entityID = do
    withEntityRigidBody entityID $ \rigidBody -> do
        withSystem physicsSystemKey $ \(PhysicsSystem dynamicsWorld) -> 
            removeRigidBody dynamicsWorld rigidBody
    
    removeComponentFromEntity rigidBodyKey entityID


withEntityRigidBody :: MonadState World m => EntityID -> (RigidBody -> m b) -> m ()
withEntityRigidBody entityID = withComponent entityID rigidBodyKey


getEntityOverlapping :: (MonadState World m, MonadIO m) => EntityID -> m [Collision]
getEntityOverlapping entityID = getComponent entityID rigidBodyKey  >>= \case
    Nothing          -> return []
    Just rigidBody -> do
        fmap (fromMaybe []) $ 
            withSystem physicsSystemKey $ \(PhysicsSystem dynamicsWorld) -> 
                contactTest dynamicsWorld rigidBody

getEntityOverlappingEntityIDs :: (MonadState World m, MonadIO m) => EntityID -> m [EntityID]
getEntityOverlappingEntityIDs entityID = 
    filter (/= entityID) 
    . concatMap (\c -> [unCollisionObjectID (cbBodyAID c), unCollisionObjectID (cbBodyBID c)]) 
    <$> getEntityOverlapping entityID


setEntitySize :: (MonadIO m, MonadState World m) => V3 GLfloat -> EntityID -> m ()
setEntitySize newSize entityID = do

    setComponent sizeKey newSize entityID

    withEntityRigidBody entityID $ \rigidBody -> do 
        withSystem physicsSystemKey $ \(PhysicsSystem dynamicsWorld) -> do
            mass       <- fromMaybe 1         <$> getComponent entityID massKey
            shapeType  <- fromMaybe CubeShape <$> getComponent entityID shapeTypeKey

            shape      <- createShapeCollider shapeType newSize
            setRigidBodyShape dynamicsWorld rigidBody shape mass



setEntityPose :: (MonadState World m, MonadIO m) => Pose GLfloat -> EntityID -> m ()
setEntityPose newPose_ entityID = do

    setComponent poseKey newPose_ entityID

    withEntityRigidBody entityID $ \rigidBody -> 
        setRigidBodyWorldTransform rigidBody (newPose_ ^. posPosition) (newPose_ ^. posOrientation)
