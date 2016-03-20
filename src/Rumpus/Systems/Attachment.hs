{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Rumpus.Systems.Attachment where
import PreludeExtra

import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import qualified Data.Set as Set
data Attachment = Attachment EntityID (M44 GLfloat) deriving (Ord, Eq)
type Attachments = Set Attachment

defineComponentKey ''Attachments

initAttachmentSystem :: (MonadIO m, MonadState ECS m) => m ()
initAttachmentSystem = do
    registerComponent "Attachments" cmpAttachments (newComponentInterface cmpAttachments)

tickAttachmentSystem :: (MonadIO m, MonadState ECS m) => m ()
tickAttachmentSystem =
    forEntitiesWithComponent cmpAttachments $
        \(entityID, attachments) -> 
            forM_ attachments $ \(Attachment toEntityID offset) -> do
                pose <- getEntityPose entityID
                setEntityPose (pose `addMatrix` offset) toEntityID

attachEntity :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> Bool -> m ()
attachEntity entityID toEntityID exclusive = do
    -- Detach any current attachments
    when exclusive $ 
        detachAttachedEntities entityID

    entityPose   <- getEntityPose entityID
    toEntityPose <- getEntityPose toEntityID
    let offset = toEntityPose `subtractMatrix` entityPose
    
    addAttachmentToSet entityID (Attachment toEntityID offset)

    withEntityRigidBody toEntityID $ \rigidBody ->
        setRigidBodyKinematic rigidBody True

detachAttachedEntities :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
detachAttachedEntities entityID =
    withAttachments entityID $ \attachments -> do
        forM_ attachments $ \(Attachment attachedEntityID _offset) -> do

            physProps <- getEntityPhysicsProperties attachedEntityID
            unless (IsKinematic `elem` physProps) $ 
                withEntityRigidBody attachedEntityID $ \rigidBody ->
                    setRigidBodyKinematic rigidBody False

        removeEntityComponent cmpAttachments entityID

addAttachmentToSet :: (MonadState s m, HasComponents s) => EntityID -> Attachment -> m ()
addAttachmentToSet entityID attachment = getEntityComponent entityID cmpAttachments >>= \case
    Nothing          -> setEntityComponent cmpAttachments (Set.singleton attachment) entityID
    Just attachments -> setEntityComponent cmpAttachments (Set.insert attachment attachments) entityID

withAttachments :: MonadState ECS m => EntityID -> (Attachments -> m b) -> m ()
withAttachments entityID = withEntityComponent_ entityID cmpAttachments

addMatrix :: M44 GLfloat -> M44 GLfloat -> M44 GLfloat
addMatrix a b = a !*! b

subtractMatrix :: M44 GLfloat -> M44 GLfloat -> M44 GLfloat
subtractMatrix a b = inv44 b !*! a