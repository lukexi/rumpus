{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Rumpus.Systems.Physics where
import PreludeExtra
import Rumpus.Types
import Rumpus.Systems.Shared
import qualified Data.Map as Map

createPhysicsSystem :: IO DynamicsWorld
createPhysicsSystem = createDynamicsWorld mempty


physicsSystem :: (MonadIO m, MonadReader WorldStatic m) => m ()
physicsSystem = do
    dynamicsWorld <- view wlsDynamicsWorld
    stepSimulation dynamicsWorld 90

-- | Copy poses from Bullet's DynamicsWorld into our own cmpPose components
syncPhysicsPosesSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => m ()
syncPhysicsPosesSystem = do
    -- Sync rigid bodies with entity poses
    traverseM_ (Map.toList <$> use (wldComponents . cmpRigidBody)) $ 
        \(entityID, rigidBody) -> do
            pose <- uncurry Pose <$> getBodyState rigidBody
            wldComponents . cmpPose . at entityID ?= pose

-- | Loop through the collisions for this frame and call any 
-- entities' registered collision callbacks
collisionsSystem :: WorldMonad ()
collisionsSystem = do

    -- NOTE: we get stale collisions with this method, so I've switched to the
    -- "contactTest" API which works perfectly

    -- Tell objects about any collisions
    -- dynamicsWorld <- view wlsDynamicsWorld
    -- collisions <- getCollisions dynamicsWorld
    
    -- forM_ collisions $ \collision -> do
    --     let bodyAID = (fromIntegral . unCollisionObjectID . cbBodyAID) collision
    --         bodyBID = (fromIntegral . unCollisionObjectID . cbBodyBID) collision
    --         appliedImpulse = cbAppliedImpulse collision
    --     traverseM_ (use (wldComponents . cmpOnCollision . at bodyAID)) $
    --         \onCollision -> onCollision bodyAID bodyBID appliedImpulse

    --     traverseM_ (use (wldComponents . cmpOnCollision . at bodyBID)) $
    --         \onCollision -> onCollision bodyBID bodyAID appliedImpulse

        -- name1 <- getEntityName bodyAID
        -- name2 <- getEntityName bodyBID
        -- putStrLnIO $ name1 ++ " collided with " ++ name2 ++ " : " ++ show appliedImpulse

    onCollisions <- Map.toList <$> use (wldComponents . cmpOnCollision)
    forM_ onCollisions $ \(entityID, onCollision) -> do
        collidingIDs <- getEntityOverlappingEntityIDs entityID
        forM_ collidingIDs $ \collidingID -> 
            onCollision entityID collidingID 0.1


addPhysicsComponent :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
                    => EntityID -> Entity -> m ()
addPhysicsComponent entityID entity = do

    let size           = entity ^. entSize
        shapeType      = entity ^. entShape
        physProperties = entity ^. entPhysicsProperties
        mass           = entity ^. entMass

    maybeShape <- case shapeType of
        NoShape          -> return Nothing
        CubeShape        -> Just <$> createBoxShape size
        SphereShape      -> Just <$> createSphereShape (size ^. _x)
        StaticPlaneShape -> Just <$> createStaticPlaneShape (0 :: Int)
    
    forM_ maybeShape $ \shape -> do
        
        let pose = entity ^. entPose
            collisionID = CollisionObjectID entityID
            bodyInfo = mempty { rbPosition = pose ^. posPosition
                              , rbRotation = pose ^. posOrientation
                              , rbMass     = mass
                              }

        dynamicsWorld <- view wlsDynamicsWorld
        
        rigidBody <- addRigidBody dynamicsWorld collisionID shape bodyInfo
        
        wldComponents . cmpRigidBody         . at entityID ?= rigidBody
        wldComponents . cmpPhysicsProperties . at entityID ?= physProperties

        when (NoContactResponse `elem` physProperties || IsKinematic `elem` physProperties) $ do
            setRigidBodyKinematic rigidBody True

        when (NoContactResponse `elem` physProperties) $ 
            setRigidBodyNoContactResponse rigidBody True

removePhysicsComponents :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => EntityID -> m ()
removePhysicsComponents entityID = do
    withEntityRigidBody entityID $ \rigidBody -> do
        dynamicsWorld <- view wlsDynamicsWorld
        removeRigidBody dynamicsWorld rigidBody
    wldComponents . cmpRigidBody . at entityID .= Nothing


withEntityRigidBody :: MonadState World m => EntityID -> (RigidBody -> m b) -> m ()
withEntityRigidBody entityID = useTraverseM_ (wldComponents . cmpRigidBody . at entityID)

getEntityOverlapping :: (MonadReader WorldStatic m, MonadState World m, MonadIO m) => EntityID -> m [Collision]
getEntityOverlapping entityID = use (wldComponents . cmpRigidBody . at entityID) >>= \case
    Nothing          -> return []
    Just rigidBody -> do
        dynamicsWorld <- view wlsDynamicsWorld
        contactTest dynamicsWorld rigidBody

getEntityOverlappingEntityIDs :: (MonadReader WorldStatic m, MonadState World m, MonadIO m) => EntityID -> m [EntityID]
getEntityOverlappingEntityIDs entityID = 
    filter (/= entityID) 
    . concatMap (\c -> [unCollisionObjectID (cbBodyAID c), unCollisionObjectID (cbBodyBID c)]) 
    <$> getEntityOverlapping entityID


setEntitySize :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => V3 GLfloat -> EntityID -> m ()
setEntitySize newSize entityID = do
    wldComponents . cmpSize . ix entityID .= newSize

    withEntityRigidBody entityID $ \rigidBody -> do 
        dynamicsWorld <- view wlsDynamicsWorld
        setRigidBodyScale dynamicsWorld rigidBody newSize



setEntityPose :: (MonadState World m, MonadIO m) => Pose GLfloat -> EntityID -> m ()
setEntityPose newPose_ entityID = do

    wldComponents . cmpPose . ix entityID .= newPose_

    withEntityRigidBody entityID $ \rigidBody -> 
        setRigidBodyWorldTransform rigidBody (newPose_ ^. posPosition) (newPose_ ^. posOrientation)
