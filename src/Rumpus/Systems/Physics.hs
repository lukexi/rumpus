{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Rumpus.Systems.Physics 
    ( module Rumpus.Systems.Physics
    , module Physics.Bullet
    ) where
import PreludeExtra
import Rumpus.Systems.Shared
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Controls
import Data.List (nub)
import Foreign.C

import Physics.Bullet

data PhysicsSystem = PhysicsSystem 
    { _phyDynamicsWorld  :: DynamicsWorld 
    , _phyCollisionPairs :: Map EntityID (Set EntityID)
    } deriving Show
makeLenses ''PhysicsSystem

data Property = Floating        -- ^ Sets bullet "Kinematic" flag 
              | Ghostly         -- ^ Sets bullet "NoContactResponse" flag
              | NoPhysicsShape  -- ^ Removes physics shape entirely (Rename to "Holographic"?)
              | Static          -- ^ Marks objects we don't want to grab
              | Teleportable    -- ^ Marks objects we want to allow teleportation to. Must have physics shape.
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

type Properties = [Property]


defineSystemKey ''PhysicsSystem

defineComponentKeyWithType "Mass"    [t|GLfloat|]
defineComponentKeyWithType "Gravity" [t|V3 GLfloat|]
defineComponentKeyWithType "CollisionGroup" [t|CShort|]
defineComponentKeyWithType "CollisionMask"  [t|CShort|]
defineComponentKey ''RigidBody
defineComponentKey ''SpringConstraint
defineComponentKey ''Properties


initPhysicsSystem :: (MonadIO m, MonadState ECS m) => m ()
initPhysicsSystem = do
    dynamicsWorld <- createDynamicsWorld mempty
    registerSystem sysPhysics (PhysicsSystem dynamicsWorld mempty)

    registerComponent "RigidBody" myRigidBody $ (newComponentInterface myRigidBody)
        { ciDeriveComponent = Just (deriveRigidBody dynamicsWorld)
        , ciRemoveComponent  = 
                withComponent_ myRigidBody $ \rigidBody -> do
                    removeRigidBody dynamicsWorld rigidBody
                    removeComponent myRigidBody
        }
    registerComponent "Mass"              myMass               (savedComponentInterface myMass)
    registerComponent "Gravity"           myGravity            (savedComponentInterface myGravity)
    registerComponent "Properties"        myProperties         (savedComponentInterface myProperties)
    registerComponent "SpringConstraint"  mySpringConstraint   (newComponentInterface mySpringConstraint)
    registerComponent "CollisionGroup"    myCollisionGroup     (newComponentInterface myCollisionGroup)
    registerComponent "CollisionMask"     myCollisionMask      (newComponentInterface myCollisionMask)

deriveRigidBody :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => DynamicsWorld -> m ()
deriveRigidBody dynamicsWorld = do
    properties <- fromMaybe [] <$> getComponent myProperties
    unless (NoPhysicsShape `elem` properties) $ do
        mShapeType <- getComponent myShape
        forM_ mShapeType $ \shapeType -> do

            mass <- fromMaybe 1 <$> getComponent myMass
            collisionGroup <- fromMaybe 1 <$> getComponent myCollisionGroup
            collisionMask  <- fromMaybe 1 <$> getComponent myCollisionMask
            size <- getSize
            poseM44 <- getPose
            let pose = poseFromMatrix poseM44
            
            collisionID <- CollisionObjectID <$> ask
            let bodyInfo = mempty { rbPosition       = pose ^. posPosition
                                  , rbRotation       = pose ^. posOrientation
                                  , rbMass           = mass
                                  , rbCollisionGroup = collisionGroup
                                  , rbCollisionMask  = collisionMask
                                  }
            shape     <- createShapeCollider shapeType size
            rigidBody <- addRigidBody dynamicsWorld collisionID shape bodyInfo
            myRigidBody ==> rigidBody

            -- Must happen after addRigidBody
            mGravity <- getComponent myGravity
            forM_ mGravity $ \gravity -> do
                setRigidBodyGravity rigidBody gravity
                setRigidBodyDisableDeactivation rigidBody True
            
            when (Ghostly `elem` properties || Floating `elem` properties) $ do
                setRigidBodyKinematic rigidBody True

            when (Ghostly `elem` properties) $ 
                setRigidBodyNoContactResponse rigidBody True
            
setFloating :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => Bool -> m ()
setFloating isFloating = do
    myProperties ==% nub . (Floating :)
    withRigidBody $ \rigidBody ->
        setRigidBodyKinematic rigidBody isFloating

setGhostly :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => Bool -> m ()
setGhostly isGhostly = do
    myProperties ==% nub . (Ghostly :)
    withRigidBody $ \rigidBody ->
        setRigidBodyNoContactResponse rigidBody isGhostly

tickPhysicsSystem :: (MonadIO m, MonadState ECS m) => m ()
tickPhysicsSystem = whenWorldPlaying $ do
    dynamicsWorld <- viewSystem sysPhysics phyDynamicsWorld
    vrPal         <- viewSystem sysControls ctsVRPal
    dt            <- getDeltaTime vrPal
    stepSimulationSimple dynamicsWorld dt

-- | Copy poses from Bullet's DynamicsWorld into our own myPose components
tickSyncPhysicsPosesSystem :: (MonadIO m, MonadState ECS m) => m ()
tickSyncPhysicsPosesSystem = whenWorldPlaying $ do
    -- Sync rigid bodies with entity poses
    forEntitiesWithComponent myRigidBody $
        \(entityID, rigidBody) -> runEntity entityID $ do
            pose <- uncurry Pose <$> getBodyState rigidBody
            setEntityPoseCacheScale entityID (transformationFromPose pose)



createShapeCollider :: (Fractional a, Real a, MonadIO m) => ShapeType -> V3 a -> m CollisionShape
createShapeCollider shapeType size = case shapeType of
    Cube        -> createBoxShape         size
    Sphere      -> createSphereShape      (size ^. _x / 2) -- we want diameter rather than radius to match boxes

withEntityRigidBody :: MonadState ECS m => EntityID -> (RigidBody -> m b) -> m ()
withEntityRigidBody entityID = void . withEntityComponent entityID myRigidBody

withRigidBody :: (MonadState ECS m, MonadReader EntityID m) => (RigidBody -> m b) -> m ()
withRigidBody action = do
    entityID <- ask
    withEntityRigidBody entityID action

getEntityOverlapping :: (MonadState ECS m, MonadIO m) => EntityID -> m [Collision]
getEntityOverlapping entityID = getEntityComponent entityID myRigidBody  >>= \case
    Nothing          -> return []
    Just rigidBody -> do
        fmap (fromMaybe []) $ 
            withSystem sysPhysics $ \(PhysicsSystem dynamicsWorld _) -> 
                contactTest dynamicsWorld rigidBody

castRay :: (MonadIO m, MonadState ECS m) => Ray GLfloat -> m (Maybe (RayResult GLfloat))
castRay ray = do
    dynamicsWorld <- viewSystem sysPhysics phyDynamicsWorld
    rayTestClosest dynamicsWorld ray

getEntityOverlappingEntityIDs :: (MonadState ECS m, MonadIO m) => EntityID -> m [EntityID]
getEntityOverlappingEntityIDs entityID = 
    filter (/= entityID) 
    . concatMap (\c -> [unCollisionObjectID (cbBodyAID c), unCollisionObjectID (cbBodyBID c)]) 
    <$> getEntityOverlapping entityID

setSize :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> m ()
setSize newSize = setEntitySize newSize =<< ask

setEntitySize :: (MonadIO m, MonadState ECS m) => V3 GLfloat -> EntityID -> m ()
setEntitySize newSize entityID = do

    setEntityComponent mySize newSize entityID

    -- Update the scaled pose cache
    pose <- getEntityPose entityID
    setEntityComponent myPoseScaled (pose !*! scaleMatrix newSize) entityID

    withEntityRigidBody entityID $ \rigidBody -> do 
        withSystem sysPhysics $ \(PhysicsSystem dynamicsWorld _) -> do
            mass       <- fromMaybe 1    <$> getEntityComponent entityID myMass
            shapeType  <- fromMaybe Cube <$> getEntityComponent entityID myShape

            -- FIXME this max 0.01 should be moved into bullet-mini; infinitesimal objects break the whole simulation
            shapeCollider <- createShapeCollider shapeType (max 0.01 newSize)
            setRigidBodyShape dynamicsWorld rigidBody shapeCollider mass

setPose :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => M44 GLfloat -> m ()
setPose pose = setEntityPose pose =<< ask

setEntityPose :: (MonadState ECS m, MonadIO m) => M44 GLfloat -> EntityID -> m ()
setEntityPose poseM44 entityID = do

    setEntityPoseCacheScale entityID poseM44 

    withEntityRigidBody entityID $ \rigidBody -> do
        let pose = poseFromMatrix poseM44
        setRigidBodyWorldTransform rigidBody (pose ^. posPosition) (pose ^. posOrientation)

setEntityPoseCacheScale :: MonadState ECS m => EntityID -> M44 GLfloat -> m ()
setEntityPoseCacheScale entityID poseM44 = do
    size <- getEntitySize entityID
    setEntityComponent myPose poseM44 entityID
    setEntityComponent myPoseScaled (poseM44 !*! scaleMatrix size) entityID

setPosition :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> m ()
setPosition position = do
    pose <- getPose
    setPose $ (pose & translation .~ position)

setRotation :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> GLfloat -> m ()
setRotation rotAxis rotAngle = do
    pose <- getPose
    setPose $ mkTransformation (axisAngle rotAxis rotAngle) (pose ^. translation)


getEntityProperties :: (HasComponents s, MonadState s f) => EntityID -> f Properties
getEntityProperties entityID = fromMaybe [] <$> getEntityComponent entityID myProperties

applyForce :: (Real a, MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 a -> m ()
applyForce aForce = applyForceToEntity aForce =<< ask

applyForceToEntity :: (Real a, MonadIO m, MonadState ECS m) => V3 a -> EntityID -> m ()
applyForceToEntity aForce entityID = do
    withEntityRigidBody entityID $ \rigidBody -> do
        applyCentralImpulse rigidBody aForce


getIsTeleportable :: MonadState ECS m => EntityID -> m Bool
getIsTeleportable = fmap (elem Teleportable) . getEntityProperties
