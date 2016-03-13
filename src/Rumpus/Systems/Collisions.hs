{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Rumpus.Systems.Collisions where
import PreludeExtra
import Rumpus.Systems.Physics
import Rumpus.Systems.PlayPause
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as Map

type OnCollision        = CollidedWithID -> CollisionImpulse -> EntityMonad ()
type OnCollisionStart   = OnCollision
type OnCollisionEnd     = CollidedWithID -> EntityMonad ()

type CollidedWithID     = EntityID
type CollisionImpulse   = GLfloat

defineComponentKey ''OnCollision
defineComponentKey ''OnCollisionStart
defineComponentKey ''OnCollisionEnd

initCollisionsSystem :: MonadState ECS m => m ()
initCollisionsSystem = do
    registerComponent "OnCollision"      cmpOnCollision      (newComponentInterface cmpOnCollision)
    registerComponent "OnCollisionStart" cmpOnCollisionStart (newComponentInterface cmpOnCollisionStart)
    registerComponent "OnCollisionEnd"   cmpOnCollisionEnd   (newComponentInterface cmpOnCollisionEnd)

-- | Loop through the collisions for this frame and call any 
-- entities' registered collision callbacks
tickCollisionsSystem :: ECSMonad ()
tickCollisionsSystem = do
    isPlaying <- viewSystem sysPlayPause plyPlaying
    if isPlaying 
        then do
            -- NOTE: we get stale collisions with bullet-mini's getCollisions, 
            -- so I've switched to the "contactTest" API which works.
            
            -- NOTE: duplicating some work here for code simplicty; should really 
            -- only do calculateCollisionDiffs once.
            -- But laziness will at least protect unused Set computations.


            lastCollisionPairs <- viewSystem sysPhysics phyCollisionPairs

            forEntitiesWithComponent cmpOnCollision $ \(entityID, onCollision) -> do
                (_, _, allCollisions) <- calculateCollisionDiffs entityID lastCollisionPairs
                forM_ allCollisions $ \collidingID ->
                    runEntity entityID $ onCollision collidingID 0.1

            forEntitiesWithComponent cmpOnCollisionStart $ \(entityID, onCollisionStart) -> do
                (newCollisions, _, _) <- calculateCollisionDiffs entityID lastCollisionPairs
                forM_ newCollisions $ \collidingID ->
                    runEntity entityID $ onCollisionStart collidingID 0.1
            
            forEntitiesWithComponent cmpOnCollisionEnd $ \(entityID, onCollisionEnd) -> do
                (_, oldCollisions, _) <- calculateCollisionDiffs entityID lastCollisionPairs
                forM_ oldCollisions $ \collidingID ->
                    runEntity entityID $ onCollisionEnd collidingID
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
