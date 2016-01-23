 module NoteUpdate where
import Rumpus

update :: OnUpdate
update entityID = do    
    pose <- getEntityPose entityID
    let height = pose ^. posPosition . _y
        note = floor (height * 24 + 48) :: Int
    withPdPatch entityID $ \patch -> 
        sendPd patch "note" (Atom (fromIntegral note))
