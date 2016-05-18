{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Constraint where
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import PreludeExtra


data Constraint = RelativePositionTo EntityID (V3 GLfloat)

defineComponentKey ''Constraint

initConstraintSystem :: MonadState ECS m => m ()
initConstraintSystem = do
    registerComponent "Constraint" myConstraint $ (newComponentInterface myConstraint) {
        -- Satisfy the constraint once upon resuscitation
        ciDeriveComponent = Just
            (withComponent_ myConstraint satisfyConstraint)
        }

tickConstraintSystem :: (MonadState ECS m, MonadIO m) => m ()
tickConstraintSystem = do
    forEntitiesWithComponent myConstraint $ \(entityID, constraint) ->
        inEntity entityID (satisfyConstraint constraint)

satisfyConstraint :: (MonadReader EntityID m, MonadIO m, MonadState ECS m) => Constraint -> m ()
satisfyConstraint constraint =
    case constraint of
        RelativePositionTo parentEntityID relativePosition -> do
            parentPose <- getEntityPose parentEntityID
            let !newPosition = parentPose !*! translateMatrix relativePosition
            setPose newPosition

setEntityConstraint :: (MonadState ECS m, MonadIO m) => Constraint -> EntityID -> m ()
setEntityConstraint constraint entityID =
    setEntityComponent myConstraint constraint entityID
