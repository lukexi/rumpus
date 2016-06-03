{-# LANGUAGE DeriveAnyClass #-}
module Rumpus.Systems.Physics
    ( module Rumpus.Systems.Physics
    , module Physics.Bullet
    ) where
import PreludeExtra
import Rumpus.Systems.Shared
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Controls
import qualified Data.List as List
import Foreign.C

import Physics.Bullet

data PhysicsSystem = PhysicsSystem
    { _phyDynamicsWorld  :: DynamicsWorld
    , _phyCollisionPairs :: Map EntityID (Set EntityID)
    } deriving Show
makeLenses ''PhysicsSystem

data Property = Floating        -- ^ Sets bullet "Kinematic" flag
              | Ghostly         -- ^ Sets bullet "NoContactResponse" flag
              | Holographic     -- ^ Removes physics shape entirely
              | Ungrabbable     -- ^ Marks objects we don't want to grab
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
        , ciRemoveComponent = removeRigidBodyComponent dynamicsWorld
        }
    registerComponent "Mass"              myMass               (savedComponentInterface myMass)
    registerComponent "Gravity"           myGravity            (savedComponentInterface myGravity)
    registerComponent "Properties"        myProperties         (savedComponentInterface myProperties)
    registerComponent "SpringConstraint"  mySpringConstraint   (newComponentInterface mySpringConstraint)
    registerComponent "CollisionGroup"    myCollisionGroup     (newComponentInterface myCollisionGroup)
    registerComponent "CollisionMask"     myCollisionMask      (newComponentInterface myCollisionMask)

removeRigidBodyComponent :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                         => DynamicsWorld -> m ()
removeRigidBodyComponent dynamicsWorld = do
    withComponent_ myRigidBody $ \rigidBody -> do
        removeRigidBody dynamicsWorld rigidBody
        removeComponent myRigidBody

deriveRigidBody :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => DynamicsWorld -> m ()
deriveRigidBody dynamicsWorld = do
    -- TODO: this suggests we might want to build the behavior running
    -- ciRemoveComponent into ciDeriveComponent,
    -- just as we want to automatically run
    -- removeComponent after ciRemoveComponent
    removeRigidBodyComponent dynamicsWorld

    properties <- fromMaybe [] <$> getComponent myProperties
    unless (Holographic `elem` properties) $ do
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


setGhostly :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => Bool -> m ()
setGhostly isGhostly = do
    if isGhostly
        then do
            prependComponent myProperties [Ghostly]
            myProperties ==% List.nub
        else
            myProperties ==% List.delete Ghostly

    withRigidBody $ \rigidBody ->
        setRigidBodyNoContactResponse rigidBody isGhostly

tickPhysicsSystem :: (MonadIO m, MonadState ECS m) => m ()
tickPhysicsSystem = whenWorldPlaying $ do
    dynamicsWorld <- getDynamicsWorld
    vrPal         <- viewSystem sysControls ctsVRPal
    dt            <- getDeltaTime vrPal
    stepSimulationSimple dynamicsWorld dt

-- | Copy poses from Bullet's DynamicsWorld into our own myPose components
tickSyncPhysicsPosesSystem :: (MonadIO m, MonadState ECS m) => m ()
tickSyncPhysicsPosesSystem = whenWorldPlaying $ do
    -- Sync rigid bodies with entity poses
    forEntitiesWithComponent myRigidBody $
        \(entityID, rigidBody) -> inEntity entityID $ do
            poseM44 <- transformationFromPose . uncurry Pose <$> getBodyState rigidBody
            size <- getEntitySize entityID
            setEntityComponent myPose poseM44 entityID
            cacheScaledPose entityID poseM44 size

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
    dynamicsWorld <- getDynamicsWorld
    rayTestClosest dynamicsWorld ray

getEntityOverlappingEntityIDs :: (MonadState ECS m, MonadIO m) => EntityID -> m [EntityID]
getEntityOverlappingEntityIDs entityID =
    filter (/= entityID)
    . concatMap (\c -> map unCollisionObjectID [cbBodyAID c, cbBodyBID c])
    <$> getEntityOverlapping entityID

getDynamicsWorld :: MonadState ECS m => m DynamicsWorld
getDynamicsWorld = viewSystem sysPhysics phyDynamicsWorld

setShape :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => ShapeType -> m ()
setShape newShape = do
    myShape ==> newShape
    dynamicsWorld <- getDynamicsWorld
    deriveRigidBody dynamicsWorld

setEntitySize :: (MonadIO m, MonadState ECS m) => EntityID -> V3 GLfloat -> m ()
setEntitySize entityID = inEntity entityID . setSize

-- Yet another hack for release - this is specifically to make it so
-- that the creator, which animates new objects to full size, can have that animation
-- "rerouted" to the new size if the object's start function sets one.
setSize :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> m ()
setSize newSize = do
    maybeAnim <- getComponent mySizeAnimation
    case maybeAnim of
        Nothing -> setSizeNoAnim newSize
        Just sizeAnim -> do
            newAnim <- redirectAnimation sizeAnim newSize
            mySizeAnimation ==> newAnim

setSizeNoAnim :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> m ()
setSizeNoAnim newSize = ask >>= \eid -> setEntitySizeNoAnim eid newSize

setEntitySizeNoAnim :: (MonadIO m, MonadState ECS m) => EntityID -> V3 GLfloat -> m ()
setEntitySizeNoAnim entityID newSize  = do

    setEntityComponent mySize newSize entityID

    -- Update the scaled pose cache
    poseM44 <- getEntityPose entityID
    cacheScaledPose entityID poseM44 newSize

    setEntityRigidBodySize entityID newSize

setEntityRigidBodySize :: (Fractional a, Real a, MonadIO m, MonadState ECS m) => EntityID -> V3 a -> m ()
setEntityRigidBodySize entityID newSize = do
    withEntityRigidBody entityID $ \rigidBody -> do
        withSystem sysPhysics $ \(PhysicsSystem dynamicsWorld _) -> do
            mass       <- fromMaybe 1    <$> getEntityComponent entityID myMass
            shapeType  <- fromMaybe Cube <$> getEntityComponent entityID myShape

            collisionGroup <- fromMaybe 1 <$> getEntityComponent entityID myCollisionGroup
            collisionMask  <- fromMaybe 1 <$> getEntityComponent entityID myCollisionMask
            let bodyInfo = mempty { rbMass           = mass
                                  , rbCollisionGroup = collisionGroup
                                  , rbCollisionMask  = collisionMask
                                  }

            -- FIXME this max 0.01 should be moved into bullet-mini; infinitesimal objects break the whole simulation
            -- NOTE: we fmap the max to make sure we apply the max element-wise;
            --       a (V3 0.0001 1 1) should not pass through unaltered
            shapeCollider <- createShapeCollider shapeType (fmap (max 0.01) newSize)
            setRigidBodyShape dynamicsWorld rigidBody shapeCollider bodyInfo

setEntityRigidBodyPose :: (MonadIO m, MonadState ECS m) => EntityID -> M44 GLfloat -> m ()
setEntityRigidBodyPose entityID poseM44 = do
    withEntityRigidBody entityID $ \rigidBody -> do
        let pose = poseFromMatrix poseM44
        setRigidBodyWorldTransform rigidBody (pose ^. posPosition) (pose ^. posOrientation)

setPositionRotationSize :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                       => V3 GLfloat -> Quaternion GLfloat -> V3 GLfloat -> m ()
setPositionRotationSize position rotationQ size = do
    let poseM44 = mkTransformation rotationQ position

    mySize ==> size
    myPose ==> poseM44

    entityID <- ask
    cacheScaledPose entityID poseM44 size
    setEntityRigidBodyPose entityID poseM44
    setEntityRigidBodySize entityID size


setPose :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => M44 GLfloat -> m ()
setPose pose = ask >>= \eid -> setEntityPose eid pose

setEntityPose :: (MonadState ECS m, MonadIO m) => EntityID -> M44 GLfloat -> m ()
setEntityPose entityID poseM44  = do

    size <- getEntitySize entityID
    setEntityComponent myPose poseM44 entityID
    cacheScaledPose entityID poseM44 size

    setEntityRigidBodyPose entityID poseM44


cacheScaledPose :: MonadState ECS m => EntityID -> M44 GLfloat -> V3 GLfloat -> m ()
cacheScaledPose entityID poseM44 size = do
    setEntityComponent myPoseScaled (poseM44 !*! scaleMatrix size) entityID

setPosition :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> m ()
setPosition position = do
    pose <- getPose
    setPose $ (pose & translation .~ position)

getPosition :: (MonadState ECS m, MonadReader EntityID m) => m (V3 GLfloat)
getPosition = view translation <$> getPose

setRotation :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 GLfloat -> GLfloat -> m ()
setRotation rotAxis rotAngle = setRotationQ (axisAngle rotAxis rotAngle)

setRotationQ :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => Quaternion GLfloat -> m ()
setRotationQ rotQuat = do
    pose <- getPose
    setPose $ mkTransformation rotQuat (pose ^. translation)


getEntityProperties :: (HasComponents s, MonadState s f) => EntityID -> f Properties
getEntityProperties entityID = fromMaybe [] <$> getEntityComponent entityID myProperties

applyForce :: (Real a, MonadIO m, MonadState ECS m, MonadReader EntityID m) => V3 a -> m ()
applyForce aForce =  ask >>= \eid -> applyForceToEntity eid aForce

applyForceToEntity :: (Real a, MonadIO m, MonadState ECS m) => EntityID -> V3 a -> m ()
applyForceToEntity entityID aForce = do
    withEntityRigidBody entityID $ \rigidBody -> do
        applyCentralImpulse rigidBody aForce


getIsTeleportable :: MonadState ECS m => EntityID -> m Bool
getIsTeleportable = fmap (elem Teleportable) . getEntityProperties
