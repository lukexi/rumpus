{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Rumpus.Systems.Physics where
import PreludeExtra
import Rumpus.Systems.Shared
import Rumpus.Systems.PlayPause



data PhysicsSystem = PhysicsSystem 
    { _phyDynamicsWorld :: DynamicsWorld 
    , _phyCollisionPairs :: Map EntityID (Set EntityID)
    } deriving Show
makeLenses ''PhysicsSystem

data PhysicsProperty = IsKinematic | NoContactResponse | Static | NoPhysicsShape
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

type PhysicsProperties = [PhysicsProperty]


defineSystemKey ''PhysicsSystem

defineComponentKeyWithType "Mass"    [t|GLfloat|]
defineComponentKeyWithType "Gravity" [t|V3 GLfloat|]
defineComponentKey ''RigidBody
defineComponentKey ''SpringConstraint
defineComponentKey ''PhysicsProperties


initPhysicsSystem :: (MonadIO m, MonadState ECS m) => m ()
initPhysicsSystem = do
    dynamicsWorld <- createDynamicsWorld mempty
    registerSystem sysPhysics (PhysicsSystem dynamicsWorld mempty)

    registerComponent "RigidBody" cmpRigidBody $ (newComponentInterface cmpRigidBody)
        { ciDeriveComponent = Just (deriveRigidBody dynamicsWorld)
        , ciRemoveComponent  = 
                withComponent_ cmpRigidBody $ \rigidBody -> do
                    removeRigidBody dynamicsWorld rigidBody
                    removeComponent cmpRigidBody
        }
    registerComponent "Mass"              cmpMass (defaultComponentInterface cmpMass 1)
    registerComponent "Gravity"           cmpGravity (newComponentInterface cmpGravity)
    registerComponent "SpringConstraint"  cmpSpringConstraint (newComponentInterface cmpSpringConstraint)
    registerComponent "PhysicsProperties" cmpPhysicsProperties (savedComponentInterface cmpPhysicsProperties)

deriveRigidBody :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => DynamicsWorld -> m ()
deriveRigidBody dynamicsWorld = do
    physProperties <- fromMaybe [] <$> getComponent cmpPhysicsProperties
    unless (NoPhysicsShape `elem` physProperties) $ do
        mShapeType <- getComponent cmpShapeType
        forM_ mShapeType $ \shapeType -> do

            mass <- fromMaybe 1 <$> getComponent cmpMass
            size <- getSize
            pose <- getPose
            
            collisionID <- CollisionObjectID <$> ask
            let bodyInfo = mempty { rbPosition = pose ^. posPosition
                                  , rbRotation = pose ^. posOrientation
                                  , rbMass     = mass
                                  }
            shape     <- createShapeCollider shapeType size
            rigidBody <- addRigidBody dynamicsWorld collisionID shape bodyInfo

            -- Must happen after addRigidBody
            mGravity <- getComponent cmpGravity
            forM_ mGravity $ \gravity -> do
                setRigidBodyGravity rigidBody gravity
                setRigidBodyDisableDeactivation rigidBody True
            
            when (NoContactResponse `elem` physProperties || IsKinematic `elem` physProperties) $ do
                setRigidBodyKinematic rigidBody True

            when (NoContactResponse `elem` physProperties) $ 
                setRigidBodyNoContactResponse rigidBody True
            
            cmpRigidBody ==> rigidBody

tickPhysicsSystem :: (MonadIO m, MonadState ECS m) => m ()
tickPhysicsSystem = whenWorldPlaying $ do
    dynamicsWorld <- viewSystem sysPhysics phyDynamicsWorld
    stepSimulation dynamicsWorld 60

-- | Copy poses from Bullet's DynamicsWorld into our own cmpPose components
tickSyncPhysicsPosesSystem :: (MonadIO m, MonadState ECS m) => m ()
tickSyncPhysicsPosesSystem = whenWorldPlaying $ do
    -- Sync rigid bodies with entity poses
    forEntitiesWithComponent cmpRigidBody $
        \(entityID, rigidBody) -> runEntity entityID $ do
            pose <- uncurry Pose <$> getBodyState rigidBody
            setComponent cmpPose pose



createShapeCollider :: (Fractional a, Real a, MonadIO m) => ShapeType -> V3 a -> m CollisionShape
createShapeCollider shapeType size = case shapeType of
    CubeShape        -> createBoxShape         size
    SphereShape      -> createSphereShape      (size ^. _x)
    StaticPlaneShape -> createStaticPlaneShape (0 :: Int)

withEntityRigidBody :: MonadState ECS m => EntityID -> (RigidBody -> m b) -> m ()
withEntityRigidBody entityID = void . withEntityComponent entityID cmpRigidBody


getEntityOverlapping :: (MonadState ECS m, MonadIO m) => EntityID -> m [Collision]
getEntityOverlapping entityID = getEntityComponent entityID cmpRigidBody  >>= \case
    Nothing          -> return []
    Just rigidBody -> do
        fmap (fromMaybe []) $ 
            withSystem sysPhysics $ \(PhysicsSystem dynamicsWorld _) -> 
                contactTest dynamicsWorld rigidBody

getEntityOverlappingEntityIDs :: (MonadState ECS m, MonadIO m) => EntityID -> m [EntityID]
getEntityOverlappingEntityIDs entityID = 
    filter (/= entityID) 
    . concatMap (\c -> [unCollisionObjectID (cbBodyAID c), unCollisionObjectID (cbBodyBID c)]) 
    <$> getEntityOverlapping entityID


setEntitySize :: (MonadIO m, MonadState ECS m) => V3 GLfloat -> EntityID -> m ()
setEntitySize newSize entityID = do

    setEntityComponent cmpSize newSize entityID

    withEntityRigidBody entityID $ \rigidBody -> do 
        withSystem sysPhysics $ \(PhysicsSystem dynamicsWorld _) -> do
            mass       <- fromMaybe 1         <$> getEntityComponent entityID cmpMass
            shapeType  <- fromMaybe CubeShape <$> getEntityComponent entityID cmpShapeType

            shape      <- createShapeCollider shapeType newSize
            setRigidBodyShape dynamicsWorld rigidBody shape mass

setPose :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => Pose GLfloat -> m ()
setPose pose = setEntityPose pose =<< ask

setEntityPose :: (MonadState ECS m, MonadIO m) => Pose GLfloat -> EntityID -> m ()
setEntityPose pose entityID = do

    setEntityComponent cmpPose pose entityID

    withEntityRigidBody entityID $ \rigidBody -> 
        setRigidBodyWorldTransform rigidBody (pose ^. posPosition) (pose ^. posOrientation)

getEntityPhysicsProperties :: (HasComponents s, MonadState s f) => EntityID -> f [PhysicsProperty]
getEntityPhysicsProperties entityID = fromMaybe [] <$> getEntityComponent entityID cmpPhysicsProperties

applyForce force = applyForceToEntity force =<< ask

applyForceToEntity force entityID = do
    withEntityRigidBody entityID $ \rigidBody -> do
        applyCentralImpulse rigidBody force
