{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Attachment where
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Control.Lens.Extra
import Linear.Extra

import Rumpus.Types
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Physics.Bullet

attachmentsSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => m ()
attachmentsSystem = do
    attachments <- Map.toList <$> use (wldComponents . cmpAttachment)
    forM_ attachments $ \(entityID, Attachment toEntityID offset) -> do
        pose <- getEntityPose entityID
        setEntityPose (addPoses pose offset) toEntityID


detachEntity :: (MonadState World m, MonadIO m) => EntityID -> m ()
detachEntity entityID = 
    withAttachment entityID $ \(Attachment attachedEntityID _offset) -> do
        wldComponents . cmpAttachment . at entityID .= Nothing
        withEntityRigidBody attachedEntityID $ \rigidBody ->
            setRigidBodyKinematic rigidBody False


attachEntity :: (MonadIO m, MonadState World m) => EntityID -> EntityID -> m ()
attachEntity entityID toEntityID = do

    -- Detach any current attachments
    detachEntity entityID

    entityPose   <- getEntityPose entityID
    toEntityPose <- getEntityPose toEntityID
    let offset = subtractPoses toEntityPose entityPose
    wldComponents . cmpAttachment . at entityID ?= Attachment toEntityID offset
    withEntityRigidBody toEntityID $ \rigidBody ->
        setRigidBodyKinematic rigidBody True

withAttachment :: MonadState World m => EntityID -> (Attachment -> m b) -> m ()
withAttachment entityID = useMaybeM_ (wldComponents . cmpAttachment . at entityID)
