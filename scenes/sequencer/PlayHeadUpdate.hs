module PlayHeadUpdate where
import Rumpus

update :: OnUpdate
update entityID = do
    -- y <- (+ 1) . (* 0.01) . sin <$> getNow
    x <- (/4) . flip mod' 4 . (*3) <$> getNow
    let newPose_ = newPose & posPosition .~ V3 x 1 0
    setEntityPose newPose_ entityID
    return ()
