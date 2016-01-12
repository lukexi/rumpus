{-# LANGUAGE FlexibleContexts #-}
module Wobble where
import Rumpus.Entity
import Rumpus.Types
import Linear.Extra
import Graphics.GL.Pal

update :: OnUpdate
update entityID = do
    now <- getNow
    let a     = (*5) . sin . (/10) $ now
        spatX = (*a) . sin  $ now
        spatZ = (*a) . cos  $ now
        newPose_ = Pose (V3 spatX 0.56  spatZ) (axisAngle (V3 0 1 0) (now + (pi/2)))
    setEntityPose newPose_ entityID
