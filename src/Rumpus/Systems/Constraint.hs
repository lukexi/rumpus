{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Constraint where
import Rumpus.Types
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import PreludeExtra


constraintSystem :: (MonadState World m, MonadIO m, MonadReader WorldStatic m) => m ()
constraintSystem = do
    useMapM_ (wldComponents . cmpConstraint) $ \(entityID, constraint) -> do
        case constraint of
            RelativePositionTo parentEntityID relativePosition -> do
                parentPose <- getEntityPose parentEntityID
                let newPosition = newPose 
                        & posPosition .~ parentPose ^. posPosition + relativePosition
                setEntityPose newPosition entityID

setEntityConstraint :: MonadState World m => Constraint -> EntityID -> m ()
setEntityConstraint constraint entityID = 
    wldComponents . cmpConstraint . at entityID ?= constraint
