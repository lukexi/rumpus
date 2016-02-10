{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Attachment where
import PreludeExtra

import Rumpus.Types
import Rumpus.ECS
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics

data Attachment = Attachment EntityID (Pose GLfloat)

defineComponentKey ''Attachment


tickAttachmentsSystem :: (MonadIO m, MonadState World m) => m ()
tickAttachmentsSystem = 
    forEntitiesWithComponent attachmentKey $
        \(entityID, Attachment toEntityID offset) -> do
            pose <- getEntityPose entityID
            setEntityPose (pose `addPose` offset) toEntityID


attachEntity :: (MonadIO m, MonadState World m) => EntityID -> EntityID -> m ()
attachEntity entityID toEntityID = do
    -- Detach any current attachments
    detachEntity entityID

    entityPose   <- getEntityPose entityID
    toEntityPose <- getEntityPose toEntityID
    let offset = toEntityPose `subtractPose` entityPose
    addComponent attachmentKey (Attachment toEntityID offset) entityID
    withEntityRigidBody toEntityID $ \rigidBody ->
        setRigidBodyKinematic rigidBody True



detachEntity :: (MonadState World m, MonadIO m) => EntityID -> m ()
detachEntity entityID = 
    withAttachment entityID $ \(Attachment attachedEntityID _offset) -> do

        removeComponentFromEntity attachmentKey entityID

        physProps <- fromMaybe [] <$> getComponent attachedEntityID physicsPropertiesKey
        unless (IsKinematic `elem` physProps) $ 
            withEntityRigidBody attachedEntityID $ \rigidBody ->
                setRigidBodyKinematic rigidBody False

withAttachment :: MonadState World m => EntityID -> (Attachment -> m b) -> m ()
withAttachment entityID = withComponent entityID attachmentKey 

