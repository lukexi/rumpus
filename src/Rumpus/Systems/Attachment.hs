{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Attachment where
import PreludeExtra

import Rumpus.Systems.Shared
import Rumpus.Systems.Physics

data Attachment = Attachment EntityID (M44 GLfloat)

defineComponentKey ''Attachment

initAttachmentSystem :: (MonadIO m, MonadState ECS m) => m ()
initAttachmentSystem = do
    registerComponent "Attachment" cmpAttachment (newComponentInterface cmpAttachment)

tickAttachmentSystem :: (MonadIO m, MonadState ECS m) => m ()
tickAttachmentSystem =
    forEntitiesWithComponent cmpAttachment $
        \(entityID, Attachment toEntityID offset) -> do
            pose <- getEntityPose entityID
            setEntityPose (pose `addMatrix` offset) toEntityID

attachEntity :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> m ()
attachEntity entityID toEntityID = do
    -- Detach any current attachments
    detachEntity entityID

    entityPose   <- getEntityPose entityID
    toEntityPose <- getEntityPose toEntityID
    let offset = toEntityPose `subtractMatrix` entityPose
    addEntityComponent cmpAttachment (Attachment toEntityID offset) entityID
    withEntityRigidBody toEntityID $ \rigidBody ->
        setRigidBodyKinematic rigidBody True

detachEntity :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
detachEntity entityID =
    withAttachment entityID $ \(Attachment attachedEntityID _offset) -> do

        removeEntityComponent cmpAttachment entityID

        physProps <- getEntityPhysicsProperties attachedEntityID
        unless (IsKinematic `elem` physProps) $ 
            withEntityRigidBody attachedEntityID $ \rigidBody ->
                setRigidBodyKinematic rigidBody False

withAttachment :: MonadState ECS m => EntityID -> (Attachment -> m b) -> m ()
withAttachment entityID = withEntityComponent_ entityID cmpAttachment

addMatrix :: M44 GLfloat -> M44 GLfloat -> M44 GLfloat
addMatrix a b = a !*! b

subtractMatrix :: M44 GLfloat -> M44 GLfloat -> M44 GLfloat
subtractMatrix a b = inv44 b !*! a