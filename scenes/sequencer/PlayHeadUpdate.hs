module PlayHeadUpdate where
import Rumpus

update :: OnUpdate
update entityID = do
    noteEntitiesToPlay <- getEntityGhostOverlappingEntityIDs entityID
    forM_ noteEntitiesToPlay $ \noteEntity -> do
        withPdPatch noteEntity $ \patch -> 
            sendPd patch "trigger" (Atom 1)

    -- Move the sequencer along
    x <- (/4) . flip mod' 4 <$> getNow
    let newPose_ = newPose & posPosition .~ V3 x 1 0
    setEntityPose newPose_ entityID
