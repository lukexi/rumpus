module NoteBlock where
import Rumpus

start :: Start
start = do
    setPdPatchFile "saw-voice.pd"

    let colorOn = colorHSL 0.1 0.8 0.8
        colorOff = colorHSL 0.1 0.8 0.2
    myCollisionStart ==> \hitEntityID _ -> do
        sendPd "trigger" (Atom 1)

        animation <- makeAnimation 0.2 (V4 1 0 1 1) (V4 1 1 1 1)
        myColorAnimation ==> animation

    myDrag ==> \_ -> do
        position <- getPosition
        let height = position ^. _y
            note = floor (height * 24 + 48) :: Int
        sendPd "note" (Atom (fromIntegral note))
