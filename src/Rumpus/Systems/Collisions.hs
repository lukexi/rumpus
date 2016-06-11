module Rumpus.Systems.Collisions where
import PreludeExtra
import Rumpus.Systems.Physics
import Rumpus.Systems.PlayPause
import Rumpus.Systems.CodeProtect
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

type CollisionContinues = CollidedWithID -> CollisionImpulse -> EntityMonad ()
type CollisionBegan     = CollidedWithID -> CollisionImpulse -> EntityMonad ()
type CollisionEnded       = CollidedWithID -> EntityMonad ()

type CollidedWithID     = EntityID
type CollisionImpulse   = GLfloat

defineComponentKey ''CollisionContinues
defineComponentKey ''CollisionBegan
defineComponentKey ''CollisionEnded

initCollisionsSystem :: MonadState ECS m => m ()
initCollisionsSystem = do
    registerComponent "CollisionContinues" myCollisionContinues (newComponentInterface myCollisionContinues)
    registerComponent "CollisionBegan"     myCollisionBegan     (newComponentInterface myCollisionBegan)
    registerComponent "CollisionEnded"       myCollisionEnded       (newComponentInterface myCollisionEnded)

-- | Loop through the collisions for this frame and call any
-- entities' registered collision callbacks
tickCollisionsSystem :: ECSMonad ()
tickCollisionsSystem = do
    isPlaying <- viewSystem sysPlayPause plyPlaying
    if isPlaying
        then runUserScriptsWithTimeout_ $ do
            -- NOTE: we get stale collisions with bullet-mini's getCollisions,
            -- so I've switched to the "contactTest" API which works.

            -- NOTE: duplicating some work here for code simplicity; should really
            -- only do calculateCollisionDiffs once.
            -- But laziness will at least protect unused Set computations.


            lastCollisionPairs <- viewSystem sysPhysics phyCollisionPairs

            forEntitiesWithComponent myCollisionContinues $ \(entityID, onCollisionContinues) -> do
                (_, _, allCollisions) <- calculateCollisionDiffs entityID lastCollisionPairs
                forM_ allCollisions $ \collidingID ->
                    inEntity entityID $
                        runUserFunctionProtected myCollisionContinues (onCollisionContinues collidingID 0.1)

            forEntitiesWithComponent myCollisionBegan $ \(entityID, onCollisionBegan) -> do
                (newCollisions, _, _) <- calculateCollisionDiffs entityID lastCollisionPairs
                forM_ newCollisions $ \collidingID ->
                    inEntity entityID $
                        runUserFunctionProtected myCollisionBegan (onCollisionBegan collidingID 0.1)

            forEntitiesWithComponent myCollisionEnded $ \(entityID, onCollisionEnded) -> do
                (_, oldCollisions, _) <- calculateCollisionDiffs entityID lastCollisionPairs
                forM_ oldCollisions $ \collidingID ->
                    inEntity entityID $
                        runUserFunctionProtected myCollisionEnded (onCollisionEnded collidingID)
        else do
            -- When not playing, do a collisions tick so we can still calculate intersections
            dynamicsWorld <- viewSystem sysPhysics phyDynamicsWorld
            performDiscreteCollisionDetection dynamicsWorld

calculateCollisionDiffs :: (MonadIO m, MonadState ECS m)
                        => EntityID
                        -> Map EntityID (Set EntityID)
                        -> m (Set EntityID, Set EntityID, Set EntityID)
calculateCollisionDiffs entityID lastCollisionPairs = modifySystemState sysPhysics $ do
    collidingIDs <- lift $ getEntityOverlappingEntityIDs entityID

    let currentCollisions = Set.fromList collidingIDs
        lastCollisions = fromMaybe Set.empty (Map.lookup entityID lastCollisionPairs)
        newCollisions = Set.difference currentCollisions lastCollisions
        oldCollisions = Set.difference lastCollisions currentCollisions
    -- This is redundantly called in the quick-and-dirty implementation but should be idempotent
    phyCollisionPairs . at entityID ?= currentCollisions
    return (newCollisions, oldCollisions, currentCollisions)
