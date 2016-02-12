{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Attachment where
import PreludeExtra

import Data.ECS
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics

data Attachment = Attachment EntityID (Pose GLfloat)

defineComponentKey ''Attachment



tickAttachmentsSystem :: (MonadIO m, MonadState ECS m) => m ()
tickAttachmentsSystem =
    forEntitiesWithComponent cmpAttachment $
        \(entityID, Attachment toEntityID offset) -> do
            pose <- getEntityPose entityID
            setEntityPose (pose `addPose` offset) toEntityID


attachEntity :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> m ()
attachEntity entityID toEntityID = do
    -- Detach any current attachments
    detachEntity entityID

    entityPose   <- getEntityPose entityID
    toEntityPose <- getEntityPose toEntityID
    let offset = toEntityPose `subtractPose` entityPose
    addComponent cmpAttachment (Attachment toEntityID offset) entityID
    withEntityRigidBody toEntityID $ \rigidBody ->
        setRigidBodyKinematic rigidBody True



detachEntity :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
detachEntity entityID =
    withAttachment entityID $ \(Attachment attachedEntityID _offset) -> do

        removeComponent cmpAttachment entityID

        physProps <- fromMaybe [] <$> getComponent attachedEntityID cmpPhysicsProperties
        unless (IsKinematic `elem` physProps) $ 
            withEntityRigidBody attachedEntityID $ \rigidBody ->
                setRigidBodyKinematic rigidBody False

withAttachment :: MonadState ECS m => EntityID -> (Attachment -> m b) -> m ()
withAttachment entityID = withComponent entityID cmpAttachment
