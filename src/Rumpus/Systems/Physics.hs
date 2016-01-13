{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Rumpus.Systems.Physics where
import Physics.Bullet
import Rumpus.Types
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens.Extra
import qualified Data.Map as Map
import Linear.Extra
import Rumpus.Systems.Shared
import Graphics.GL.Pal

createPhysicsSystem :: IO DynamicsWorld
createPhysicsSystem = createDynamicsWorld mempty


physicsSystem :: (MonadIO m, MonadReader WorldStatic m) => m ()
physicsSystem = do
    dynamicsWorld <- view wlsDynamicsWorld
    stepSimulation dynamicsWorld 90

syncPhysicsPosesSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => m ()
syncPhysicsPosesSystem = do
    -- Sync rigid bodies with entity poses
    traverseM_ (Map.toList <$> use (wldComponents . cmpRigidBody)) $ 
        \(entityID, rigidBody) -> do
            pose <- uncurry Pose <$> getBodyState rigidBody
            wldComponents . cmpPose . at entityID ?= pose


collisionsSystem :: WorldMonad ()
collisionsSystem = do
    dynamicsWorld <- view wlsDynamicsWorld
    -- Tell objects about any collisions
    collisions <- getCollisions dynamicsWorld
    
    forM_ collisions $ \collision -> do
        let bodyAID = (fromIntegral . unCollisionObjectID . cbBodyAID) collision
            bodyBID = (fromIntegral . unCollisionObjectID . cbBodyBID) collision
            appliedImpulse = cbAppliedImpulse collision
        traverseM_ (use (wldComponents . cmpOnCollision . at bodyAID)) $
            \onCollision -> onCollision bodyAID bodyBID appliedImpulse

        traverseM_ (use (wldComponents . cmpOnCollision . at bodyBID)) $
            \onCollision -> onCollision bodyBID bodyAID appliedImpulse



addPhysicsComponent :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
                    => EntityID -> Entity -> m ()
addPhysicsComponent entityID entity = do

    
    let size           = entity ^. entSize
        shapeType      = entity ^. entShape
        physProperties = entity ^. entPhysProps
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
        if IsGhost `elem` physProperties 
            then do
                ghostObject <- addGhostObject dynamicsWorld collisionID shape bodyInfo

                wldComponents . cmpGhostObject . at entityID ?= ghostObject

                return ()
            else do
                rigidBody <- addRigidBody dynamicsWorld collisionID shape bodyInfo
                
                when (IsKinematic `elem` physProperties) 
                    (setRigidBodyKinematic rigidBody True)

                wldComponents . cmpRigidBody . at entityID ?= rigidBody

withEntityRigidBody :: MonadState World m => EntityID -> (RigidBody -> m b) -> m ()
withEntityRigidBody entityID = useMaybeM_ (wldComponents . cmpRigidBody . at entityID)

withEntityGhostObject :: MonadState World m => EntityID -> (GhostObject -> m b) -> m ()
withEntityGhostObject entityID = useMaybeM_ (wldComponents . cmpGhostObject . at entityID)

getEntityGhostOverlapping :: (MonadState World m, MonadIO m) => EntityID -> m [CollisionObject]
getEntityGhostOverlapping entityID = use (wldComponents . cmpGhostObject . at entityID) >>= \case
    Nothing          -> return []
    Just ghostObject -> getGhostObjectOverlapping ghostObject

getEntityGhostOverlappingEntityIDs :: (MonadState World m, MonadIO m) => EntityID -> m [EntityID]
getEntityGhostOverlappingEntityIDs entityID = do
    overlappingCollisionObjects <- getEntityGhostOverlapping entityID
    map unCollisionObjectID <$> mapM getCollisionObjectID overlappingCollisionObjects


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
    withEntityGhostObject entityID $ \ghostObject -> 
        setCollisionObjectWorldTransform ghostObject (newPose_ ^. posPosition) (newPose_ ^. posOrientation)
