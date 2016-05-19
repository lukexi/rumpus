module NoteBlock where
import Rumpus

colorOn = colorHSL 0.1 0.8 0.8
colorOff = colorHSL 0.1 0.8 0.2

start :: Start
start = do

    let updateNote = do
            position <- getPosition
            let height = position ^. _y
                note = floor (height * 24 + 48) :: Int
            acquirePolyPatch "saw-voice.pd"
            sendPd "note" (fromIntegral note)

    updateNote

    myCollisionStart ==> \hitEntityID _ -> do
        acquirePolyPatch "saw-voice.pd"
        sendPd "trigger" 1

        animation <- makeAnimation 0.2 (V4 1 0 1 1) (V4 1 1 1 1)
        myColorAnimation ==> animation

    myDrag ==> \_ -> updateNote
    myCodeHidden ==> True
