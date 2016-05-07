{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Rumpus.Systems.Attachment where
import PreludeExtra

import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import qualified Data.Set as Set

data Attachment = Attachment
    { atcToEntityID :: !EntityID
    , atcOffset     :: !(M44 GLfloat)
    } deriving (Ord, Eq)
type Attachments = Set Attachment

defineComponentKey ''Attachments
defineComponentKeyWithType "Holder" [t|EntityID|]

initAttachmentSystem :: (MonadIO m, MonadState ECS m) => m ()
initAttachmentSystem = do
    registerComponent "Attachments" myAttachments (newComponentInterface myAttachments)
    registerComponent "Holder" myHolder $ (newComponentInterface myHolder)
        -- Haven't needed this yet, but defining it since it's logical enough
        { ciDeriveComponent = Just $ do
            withComponent_ myHolder $ \holderID -> do
                entityID <- ask
                attachEntity holderID entityID False
        , ciRemoveComponent = detachFromHolder >> removeComponent myHolder
        }

tickAttachmentSystem :: (MonadIO m, MonadState ECS m) => m ()
tickAttachmentSystem =
    forEntitiesWithComponent myAttachments $
        \(entityID, attachments) ->
            forM_ attachments $ \(Attachment toEntityID offset) -> do
                pose <- getEntityPose entityID
                setEntityPose toEntityID (pose `addMatrix` offset)

detachFromHolder :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => m ()
detachFromHolder = detachEntityFromHolder =<< ask

detachEntityFromHolder :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
detachEntityFromHolder entityID = do
    traverseM_ (getEntityComponent entityID myHolder) $ \holderID -> do
        detachAttachedEntity holderID entityID

attachEntity :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> Bool -> m ()
attachEntity holderEntityID toEntityID exclusive = do

    detachEntityFromHolder toEntityID

    -- Detach any current attachments
    when exclusive $
        detachAttachedEntities holderEntityID

    entityPose   <- getEntityPose holderEntityID
    toEntityPose <- getEntityPose toEntityID
    let offset = toEntityPose `subtractMatrix` entityPose

    addAttachmentToSet holderEntityID (Attachment toEntityID offset)
    runEntity toEntityID (myHolder ==> holderEntityID)
    overrideSetKinematicMode toEntityID

detachAttachedEntity :: (MonadState ECS m, MonadIO m) => EntityID -> EntityID -> m ()
detachAttachedEntity holderEntityID entityID = do
    restoreSetKinematicMode entityID
    modifyEntityComponent holderEntityID myAttachments
        (Set.filter (not . (== entityID) . atcToEntityID))

-- | Force kinematic mode to on to allow objects to be carried
overrideSetKinematicMode :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
overrideSetKinematicMode entityID =
    withEntityRigidBody entityID $ \rigidBody ->
        setRigidBodyKinematic rigidBody True

-- | Restores the kinematic mode requested in the entity's myProperties
restoreSetKinematicMode :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
restoreSetKinematicMode entityID = do
    properties <- getEntityProperties entityID
    unless (Floating `elem` properties) $
        withEntityRigidBody entityID $ \rigidBody ->
            setRigidBodyKinematic rigidBody False

detachAttachedEntities :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
detachAttachedEntities holderEntityID =
    withAttachments holderEntityID $ \attachments -> do
        forM_ attachments $ \(Attachment attachedEntityID _offset) -> do
            restoreSetKinematicMode attachedEntityID

        removeEntityComponent myAttachments holderEntityID

addAttachmentToSet :: (MonadState ECS m) => EntityID -> Attachment -> m ()
addAttachmentToSet entityID attachment =
    appendEntityComponent entityID myAttachments (Set.singleton attachment)

withAttachments :: MonadState ECS m => EntityID -> (Attachments -> m b) -> m ()
withAttachments entityID = withEntityComponent_ entityID myAttachments

getEntityAttachments :: (MonadState ECS m) => EntityID -> m (Maybe Attachments)
getEntityAttachments entityID = getEntityComponent entityID myAttachments

isEntityAttachedTo :: (MonadState ECS m) => EntityID -> EntityID -> m Bool
isEntityAttachedTo childID parentID = maybe False (Set.member childID . Set.map atcToEntityID) <$> getEntityAttachments parentID


