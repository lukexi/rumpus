module Note where
import Rumpus

colorOn = colorHSL 0.1 0.8 0.8
colorOff = colorHSL 0.1 0.8 0.2

noteBase :: Num a => a
noteBase = 48
notesPerMeter :: Num a => a
notesPerMeter = 24

start :: Start
start = do
    setBody Animated
    setSize (V3 0.2 (1/24) 0.3)

    setState (0::Int)

    let noteHue note = fromIntegral note / notesPerMeter

    let updateNote = editState $ \currentNote -> do
            position <- getPosition
            let height = position ^. _y
                note = floor (height * notesPerMeter + noteBase) :: Int

            when (note /= currentNote) $ do
                setColor (colorHSL (noteHue note) 0.8 0.5)
                acquirePolyPatch "Note.pd"
                sendSynth "note" (fromIntegral note)
                sendSynth "trigger" 1
            return note


    updateNote

    myCollisionBegan ==> \hitEntityID _ -> do
        spawnBall
        note <- getState (0::Int)
        acquirePolyPatch "Note.pd"
        sendSynth "note" (fromIntegral note)
        sendSynth "trigger" 1


        let hue = noteHue note
            fromColor = colorHSL (hue + 0.5) 0.8 0.5
            toColor   = colorHSL hue         0.8 0.5
        animateColor fromColor toColor 0.2

    myDragContinues ==> \_ -> updateNote
    myCodeHidden ==> True

spawnBall = do
    noteColor <- getColor
    notePosition <- getPosition
    spawnChild $ do
        myLifetime ==> 3
        myShape    ==> Sphere
        mySize     ==> 0.1
        myBody     ==> Physical
        myPose     ==> postiion (notePosition + (V3 0 -1/24 0))
