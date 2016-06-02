module NoteUpdate where
import Rumpus

update :: Update
update entityID = do
    pose <- getEntityPose entityID
    let height = pose ^. posPosition . _y
        note = floor (height * 24 + 48) :: Int

    sendEntitySynth entityID "note" (Atom (fromIntegral note))
