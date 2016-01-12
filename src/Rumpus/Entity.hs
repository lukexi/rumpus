{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Rumpus.Entity where
import Control.Lens.Extra
import Linear.Extra
import Graphics.GL.Pal
import Physics.Bullet
import Control.Monad.State
import System.Random
import Control.Monad.Reader
-- import Data.Maybe

import Sound.Pd
import TinyRick

import Rumpus.Types

spawnEntity entityName = 
    traverseM (use (wldEntityLibrary . at entityName)) createEntity

defineEntity entity = wldEntityLibrary . at (entity ^. entName) ?= entity

createEntity :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => Entity -> m EntityID
createEntity entity = do
    entityID <- liftIO randomIO

    wldScene . at entityID ?= entity
    

    wldComponents . cmpPose      . at entityID ?= entity ^. entPose
    wldComponents . cmpSize      . at entityID ?= entity ^. entSize
    wldComponents . cmpColor     . at entityID ?= entity ^. entColor
    wldComponents . cmpScale     . at entityID ?= entity ^. entScale
    wldComponents . cmpShape     . at entityID ?= entity ^. entShape
    wldComponents . cmpName      . at entityID ?= entity ^. entName

    addScriptComponent  entityID entity

    addPhysicsComponent entityID entity
    
    addPdPatchComponent entityID entity

    forM_ (entity ^. entChildren) $ \child -> do
        childID <- createEntity child
        wldComponents . cmpParent . at childID ?= entityID
    
    return entityID

addPdPatchComponent entityID entity = forM_ (entity ^. entPdPatch) $ \patchPath -> do
    pd <- view wlsPd
    patch <- makePatch pd patchPath
    wldComponents . cmpPdPatch . at entityID ?= patch
    -- Assign the patch's output DAC index to route it to the the SourceID
    dequeueOpenALSource >>= mapM_ (\(sourceChannel, _sourceID) -> do
        send pd patch "dac" (Atom (Float (fromIntegral sourceChannel)))
        )

addScriptComponent entityID entity = forM_ (entity ^. entScript) $ \scriptPath -> do
    ghcChan <- view wlsGHCChan
    font    <- view wlsFont
    editor  <- liftIO $ 
        makeExpressionEditor ghcChan font scriptPath "update" 
            (return . const ()) 
            (identity & translation .~ V3 0 0 0)
    
    wldComponents . cmpScript . at entityID ?= editor

dequeueOpenALSource :: MonadState World m => m (Maybe (Int, OpenALSource))
dequeueOpenALSource = do
    sources <- use wldOpenALSourcePool
    case sources of
        [] -> return Nothing
        (x:xs) -> do
            wldOpenALSourcePool .= xs ++ [x]
            return (Just x)

setEntitySize :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => EntityID -> V3 GLfloat -> m ()
setEntitySize entityID newSize = do
    wldComponents . cmpSize . ix entityID .= newSize

    withEntityRigidBody entityID $ \rigidBody -> do 
        dynamicsWorld <- view wlsDynamicsWorld
        setRigidBodyScale dynamicsWorld rigidBody newSize

setEntityColor :: (MonadState World m, MonadReader WorldStatic m) => EntityID -> V4 GLfloat -> m ()
setEntityColor entityID newColor = do
    wldComponents . cmpColor . ix entityID .= newColor

useMaybeM_ :: (MonadState s m) => Lens' s (Maybe a) -> (a -> m b) -> m ()
useMaybeM_ aLens f = do
    current <- use aLens
    mapM_ f current

withEntityRigidBody :: MonadState World m => EntityID -> (RigidBody -> m b) -> m ()
withEntityRigidBody entityID = useMaybeM_ (wldComponents . cmpRigidBody . at entityID)

withEntityGhostObject :: MonadState World m => EntityID -> (GhostObject -> m b) -> m ()
withEntityGhostObject entityID = useMaybeM_ (wldComponents . cmpGhostObject . at entityID)

getEntityGhostOverlapping :: (MonadState World m, MonadIO m) => EntityID -> m [CollisionObject]
getEntityGhostOverlapping entityID = use (wldComponents . cmpGhostObject . at entityID) >>= \case
    Nothing          -> return []
    Just ghostObject -> getGhostObjectOverlapping ghostObject

setEntityPose :: (MonadState World m, MonadIO m) => EntityID -> Pose GLfloat -> m ()
setEntityPose entityID newPose_ = do

    wldComponents . cmpPose . ix entityID .= newPose_

    withEntityRigidBody entityID $ \rigidBody -> 
        setRigidBodyWorldTransform rigidBody (newPose_ ^. posPosition) (newPose_ ^. posOrientation)
    withEntityGhostObject entityID $ \ghostObject -> 
        setCollisionObjectWorldTransform ghostObject (newPose_ ^. posPosition) (newPose_ ^. posOrientation)


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

