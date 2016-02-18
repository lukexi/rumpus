module PlayHeadUpdate where
import Rumpus

update :: OnUpdate
update = do
    -- y <- (+ 1) . (* 0.01) . sin <$> getNow
    x <- (/4) . flip mod' 4 . (*3) <$> getNow
    setPose $ newPose & posPosition .~ V3 x 1 0
