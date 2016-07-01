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
    setSize (V3 (1/4) (1/24) (1/4))

    setState (0::Int)

    let noteHue note = fromIntegral note / notesPerMeter

    let updateNote play = editState $ \currentNote -> do
            position <- getPosition
            let height = position ^. _y
                note = floor (height * notesPerMeter + noteBase) :: Int

            when (play && note /= currentNote) $ do
                setColor (colorHSL (noteHue note) 0.8 0.5)
                acquirePolyPatch "Note.pd"
                sendSynth "note" (fromIntegral note)
                sendSynth "trigger" 1
            return note


    updateNote False

    myCollisionBegan ==> \hitEntityID _ -> do
        shape <- getEntityComponentDefault Cube hitEntityID myShape
        when (shape /= Sphere) $ do
--            spawnBall
            note <- getState (0::Int)
            acquirePolyPatch "Note.pd"
            sendSynth "note" (fromIntegral note)
            sendSynth "trigger" 1


            let hue = noteHue note
                fromColor = colorHSL (hue + 0.5) 0.8 0.5
                toColor   = colorHSL hue         0.8 0.5
            animateColor fromColor toColor 0.2

    myDragContinues ==> \_ -> updateNote True

    -- Quantize note position on release
    myDragEnded ==> do
        V3 x y z <- getPosition
        let quantPos = V3
                (quantizeToF (1/8) x)
                (quantizeToF (1/24) y)
                (quantizeToF (1/8) z)
        animatePositionTo quantPos 0.3

        V3 x y z <- getRotationEuler
        let quantRot = eulerToQuat $ V3
                (quantizeToF (pi/4) x)
                (quantizeToF (pi/4) y)
                (quantizeToF (pi/4) z)
        animateRotationTo quantRot 0.3
        -- Save quantized position
        setDelayedAction 0.3 (sceneWatcherSaveEntity =<< ask)

    myCodeHidden ==> True



spawnBall :: EntityMonad ()
spawnBall = do
    noteColor <- getColor
    notePosition <- getPosition
    spawnChild_ $ do
        myLifetime ==> 3
        myShape    ==> Sphere
        mySize     ==> 0.1
        myBody     ==> Physical
        myColor    ==> noteColor
        myPose     ==> position (notePosition + (V3 0 (-0.05) 0))
