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
-- import Data.Maybe
import Types

createEntity :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => Entity -> m EntityID
createEntity entity = do
    entityID <- liftIO randomIO

    wldComponents . cmpPose   . at entityID ?= entity ^. entPose
    wldComponents . cmpSize   . at entityID ?= entity ^. entSize
    wldComponents . cmpColor  . at entityID ?= entity ^. entColor
    wldComponents . cmpScale  . at entityID ?= entity ^. entScale
    wldComponents . cmpShape  . at entityID ?= entity ^. entShape
    wldComponents . cmpUpdate . at entityID .= entity ^. entUpdate

    addEntityRigidBodyComponent entityID entity

    forM_ (entity ^. entChildren) $ \child -> do
        childID <- createEntity child
        wldComponents . cmpParents . at childID ?= entityID
    
    return entityID


setEntitySize :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => EntityID -> V3 GLfloat -> m ()
setEntitySize entityID newSize = do
    wldComponents . cmpSize . ix entityID .= newSize

    useMaybeM_ (wldComponents . cmpRigidBody . at entityID) $ \rigidBody -> do 
        dynamicsWorld <- view wlsDynamicsWorld
        setRigidBodyScale dynamicsWorld rigidBody newSize

setEntityColor :: (MonadState World m, MonadReader WorldStatic m) => EntityID -> V4 GLfloat -> m ()
setEntityColor entityID newColor = do
    wldComponents . cmpColor . ix entityID .= newColor

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
                            => EntityID -> Entity -> m ()
addEntityRigidBodyComponent entityID entity = do

    
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

        let collisionID = CollisionObjectID entityID
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
                    (setRigidBodyKinematic rigidBody)

                wldComponents . cmpRigidBody . at entityID ?= rigidBody


