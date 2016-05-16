module NoteBlock where
import Rumpus

start :: Start
start = do
    let colorOn = colorHSL 0.1 0.8 0.8
        colorOff = colorHSL 0.1 0.8 0.2
    myCollisionStart ==> \hitEntityID _ -> do
        setColor colorOn
        y <- view translation <$> getEntityPose
        let note = y * 12
        sendEntityPd hitEntityID "piano-key" (List [note, 1])
    myCollisionEnd ==> \hitEntityID -> do
        setColor colorOff
        sendEntityPd hitEntityID "piano-key" (List [note, 0])
    return ()