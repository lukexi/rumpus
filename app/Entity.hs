{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Entity where
import Control.Lens.Extra
import Linear.Extra
import Graphics.GL.Pal
import Physics.Bullet
import Control.Monad.State
import System.Random
import Control.Monad.Reader
import Data.Maybe
import Types

newWorld :: World
newWorld = World
    { _wldPlayer = Pose (V3 0 0 0) (axisAngle (V3 0 1 0) 0)
    , _wldComponents = newComponents
    , _wldEvents = []
    }

newEntity :: Entity
newEntity = Entity
    { _entColor     = V4 1 1 1 1
    , _entSize      = V3 1 1 1
    , _entPose      = newPose
    , _entScale     = V3 1 1 1
    , _entRigidBody = Nothing
    , _entUpdate    = Nothing
    , _entPhysProps = []
    , _entShape     = None
    }


createEntity :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => Entity -> m EntityID
createEntity entity = do
    entityID <- liftIO randomIO

    wldComponents . cmpPose   . at entityID ?= entity ^. entPose
    wldComponents . cmpSize   . at entityID ?= entity ^. entSize
    wldComponents . cmpColor  . at entityID ?= entity ^. entColor
    wldComponents . cmpScale  . at entityID ?= entity ^. entScale
    wldComponents . cmpShape  . at entityID ?= entity ^. entShape
    wldComponents . cmpUpdate . at entityID .= entity ^. entUpdate

    addEntityRigidBodyComponent entityID (entity ^. entPhysProps)
    
    return entityID


setEntitySize :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => EntityID -> V3 GLfloat -> m ()
setEntitySize entityID newSize = do
    wldComponents . cmpSize . ix entityID .= newSize

    useMaybeM_ (wldComponents . cmpRigidBody . at entityID) $ \rigidBody -> do 
        dynamicsWorld <- view wlsDynamicsWorld
        setRigidBodyScale dynamicsWorld rigidBody newSize


useMaybeM_ :: (MonadState s m) => Lens' s (Maybe a) -> (a -> m b) -> m ()
useMaybeM_ aLens f = do
    current <- use aLens
    mapM_ f current


setEntityPose :: (MonadState World m, MonadIO m) => EntityID -> Pose GLfloat -> m ()
setEntityPose entityID newPose_ = do

    wldComponents . cmpPose . ix entityID .= newPose_

    useMaybeM_ (wldComponents . cmpRigidBody . at entityID) $ \rigidBody -> 
        setRigidBodyWorldTransform rigidBody (newPose_ ^. posPosition) (newPose_ ^. posOrientation)



addEntityRigidBodyComponent :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
                            => EntityID -> [PhysicsProperties] -> m ()
addEntityRigidBodyComponent entityID physProperties = do

    dynamicsWorld <- view wlsDynamicsWorld

    pose <- fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)
    size <- fromMaybe 1       <$> use (wldComponents . cmpSize . at entityID)

    boxShape <- createBoxShape size

    let collisionID = CollisionObjectID entityID
        bodyInfo = mempty { rbPosition = pose ^. posPosition
                          , rbRotation = pose ^. posOrientation
                          }

    if IsGhost `elem` physProperties 
        then do
            ghostObject <- addGhostObject dynamicsWorld collisionID boxShape bodyInfo

            wldComponents . cmpGhostObject . at entityID ?= ghostObject

            return ()
        else do
            rigidBody <- addRigidBody dynamicsWorld collisionID boxShape bodyInfo
            
            when (IsKinematic `elem` physProperties) 
                (setRigidBodyKinematic rigidBody)

            wldComponents . cmpRigidBody . at entityID ?= rigidBody


