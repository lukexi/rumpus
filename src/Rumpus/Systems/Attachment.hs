module Rumpus.Systems.Attachment where
import PreludeExtra

import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import qualified Data.HashMap.Strict as Map

type Attachments = Map EntityID (M44 GLfloat)

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
                attachEntityToEntityAtCurrentOffset holderID entityID
        , ciRemoveComponent = detachFromHolder >> removeComponent myHolder
        }

tickAttachmentSystem :: (MonadIO m, MonadState ECS m) => m ()
tickAttachmentSystem =
    forEntitiesWithComponent myAttachments $
        \(holderID, attachments) ->
            forM_ (Map.toList attachments) $ \(toEntityID, offset) -> do
                setPoseFromAttachment holderID toEntityID offset

setPoseFromAttachment holderID toEntityID offset = do
    pose <- getEntityPose holderID
    setEntityPose toEntityID (pose `addMatrix` offset)

detachFromHolder :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => m ()
detachFromHolder = detachEntityFromHolder =<< ask

detachEntityFromHolder :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
detachEntityFromHolder entityID = do
    traverseM_ (getEntityComponent entityID myHolder) $ \holderID -> do
        detachAttachedEntity holderID entityID

setAttachmentOffset :: (MonadState ECS m, MonadReader EntityID m)
                    => M44 GLfloat -> m ()
setAttachmentOffset newOffset = do
    withComponent_ myHolder $ \holderID -> do
        myID <- ask
        modifyEntityComponent holderID myAttachments (Map.adjust (const newOffset) myID)

attachEntityAtCurrentOffset :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => EntityID -> m ()
attachEntityAtCurrentOffset toEntityID = do
    holderID <- ask
    attachEntityToEntityAtCurrentOffset holderID toEntityID

attachEntityToEntityAtCurrentOffset :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> m ()
attachEntityToEntityAtCurrentOffset holderID toEntityID = do
    entityPose   <- getEntityPose holderID
    toEntityPose <- getEntityPose toEntityID
    let offset = toEntityPose `subtractMatrix` entityPose
    attachEntityToEntity holderID toEntityID offset

attachEntity :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => EntityID -> M44 GLfloat -> m ()
attachEntity toEntityID offset = do
    holderID <- ask
    attachEntityToEntity holderID toEntityID offset

attachEntityToEntity :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> M44 GLfloat -> m ()
attachEntityToEntity holderID toEntityID offset = do

    detachEntityFromHolder toEntityID

    appendAttachment holderID toEntityID offset
    inEntity toEntityID (myHolder ==> holderID)
    overrideSetKinematicMode toEntityID

    setPoseFromAttachment holderID toEntityID offset

detachAttachedEntity :: (MonadState ECS m, MonadIO m) => EntityID -> EntityID -> m ()
detachAttachedEntity holderID entityID = do
    restoreSetKinematicMode entityID
    modifyEntityComponent holderID myAttachments (Map.delete entityID)
    removeEntityComponent myHolder entityID

-- | Force kinematic mode to on to allow objects to be carried
overrideSetKinematicMode :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
overrideSetKinematicMode entityID =
    withEntityRigidBody entityID $ \rigidBody ->
        setRigidBodyKinematic rigidBody True

-- | Restores the kinematic mode requested in the entity's myBodyFlags
restoreSetKinematicMode :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
restoreSetKinematicMode entityID = do
    mBodyType <- getEntityBody entityID
    unless (mBodyType == Just Animated) $
        withEntityRigidBody entityID $ \rigidBody ->
            setRigidBodyKinematic rigidBody False

detachAttachedEntities :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
detachAttachedEntities holderID =
    withAttachments holderID $ \attachments -> do
        forM_ (Map.keys attachments) $ \attachedEntityID -> do
            restoreSetKinematicMode attachedEntityID
            removeEntityComponent myHolder attachedEntityID

        removeEntityComponent myAttachments holderID

appendAttachment :: (MonadState ECS m) => EntityID -> EntityID -> M44 GLfloat -> m ()
appendAttachment holderID entityID offset =
    appendEntityComponent holderID myAttachments (Map.singleton entityID offset)

withAttachments :: MonadState ECS m => EntityID -> (Attachments -> m b) -> m ()
withAttachments entityID = withEntityComponent_ entityID myAttachments

getEntityAttachments :: (MonadState ECS m) => EntityID -> m (Map EntityID (M44 GLfloat))
getEntityAttachments entityID = getEntityComponentDefault mempty entityID myAttachments

isEntityAttachedTo :: (MonadState ECS m) => EntityID -> EntityID -> m Bool
isEntityAttachedTo childID parentID = Map.member childID <$> getEntityAttachments parentID


getOneEntityAttachment :: MonadState ECS m => EntityID -> m (Maybe EntityID)
getOneEntityAttachment entityID = listToMaybe . Map.keys <$> getEntityAttachments entityID


hasHolder :: (MonadState ECS m, MonadReader EntityID m) => m Bool
hasHolder = hasComponent myHolder

setBody :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => BodyType -> m ()
setBody bodyType = do
    hadBody <- hasComponent myBody
    myBody ==> bodyType

    if hadBody
        then withRigidBody $ \rigidBody ->
            updateRigidBodyWithBodyType rigidBody bodyType
        else deriveRigidBody =<< getDynamicsWorld

    -- Ensure body stays kinematic when being held
    isHeld <- hasHolder
    when isHeld $
        withRigidBody $ \rigidBody ->
            setRigidBodyKinematic rigidBody True
