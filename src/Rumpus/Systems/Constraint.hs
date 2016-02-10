{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Constraint where
import Rumpus.Types
import Rumpus.ECS
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import PreludeExtra


data Constraint = RelativePositionTo EntityID (V3 GLfloat)

defineComponentKey ''Constraint

tickConstraintSystem :: (MonadState World m, MonadIO m) => m ()
tickConstraintSystem = do
    forEntitiesWithComponent constraintKey $ \(entityID, constraint) -> do
        case constraint of
            RelativePositionTo parentEntityID relativePosition -> do
                parentPose <- getEntityPose parentEntityID
                let newPosition = newPose 
                        & posPosition .~ parentPose ^. posPosition + relativePosition
                setEntityPose newPosition entityID

setEntityConstraint :: (MonadState World m, MonadIO m) => Constraint -> EntityID -> m ()
setEntityConstraint constraint entityID = 
    addComponent constraintKey constraint entityID
