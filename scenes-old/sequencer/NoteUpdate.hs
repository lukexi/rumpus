module NoteUpdate where
import Rumpus

update :: OnUpdate
update entityID = do    
    pose <- getEntityPose entityID
    let height = pose ^. posPosition . _y
        note = floor (height * 24 + 48) :: Int
    
    sendEntityPd entityID "note" (Atom (fromIntegral note))
