{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Constraint where
import Data.ECS
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import PreludeExtra


data Constraint = RelativePositionTo EntityID (V3 GLfloat)

defineComponentKey ''Constraint

initConstraintSystem :: MonadState ECS m => m ()
initConstraintSystem = do
    registerComponent "Constraint" cmpConstraint $ (newComponentInterface cmpConstraint) { 
        -- Satisfy the constraint once upon resuscitation
        ciDeriveComponent = Just (\entityID -> 
            withComponent entityID cmpConstraint $ satisfyConstraint entityID)
        }

tickConstraintSystem :: (MonadState ECS m, MonadIO m) => m ()
tickConstraintSystem = do
    forEntitiesWithComponent cmpConstraint $ \(entityID, constraint) -> satisfyConstraint entityID constraint

satisfyConstraint :: (MonadIO m, MonadState ECS m) => EntityID -> Constraint -> m ()
satisfyConstraint entityID constraint =
    case constraint of
        RelativePositionTo parentEntityID relativePosition -> do
            parentPose <- getEntityPose parentEntityID
            let newPosition = newPose 
                    & posPosition .~ parentPose ^. posPosition + relativePosition
            setEntityPose newPosition entityID

setEntityConstraint :: (MonadState ECS m, MonadIO m) => Constraint -> EntityID -> m ()
setEntityConstraint constraint entityID = 
    setComponent cmpConstraint constraint entityID
