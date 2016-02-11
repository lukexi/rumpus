{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Constraint where
import Rumpus.Types
import Data.ECS
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import PreludeExtra


data Constraint = RelativePositionTo EntityID (V3 GLfloat)

defineComponentKey ''Constraint

tickConstraintSystem :: (MonadState ECS m, MonadIO m) => m ()
tickConstraintSystem = do
    forEntitiesWithComponent cmpConstraint $ \(entityID, constraint) -> do
        case constraint of
            RelativePositionTo parentEntityID relativePosition -> do
                parentPose <- getEntityPose parentEntityID
                let newPosition = newPose 
                        & posPosition .~ parentPose ^. posPosition + relativePosition
                setEntityPose newPosition entityID

setEntityConstraint :: (MonadState ECS m, MonadIO m) => Constraint -> EntityID -> m ()
setEntityConstraint constraint entityID = 
    setComponent cmpConstraint constraint entityID
