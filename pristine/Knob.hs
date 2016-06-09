module Knob where
import Rumpus
import Text.Printf

majorScale = [0,2,4,5,7,9,11,12]

start :: Start
start = do
    setSynthPatch "Verylogue.pd"

    rootID <- ask
    setState (0::Int)
    cutoffKnob <- spawnActiveKnob "Cutoff" (0, 1500) $ \n -> do
        sendEntitySynth rootID "cutoff" (realToFrac n)
    spawnActiveKnob "Resonance" (0,1)  $ \n -> do
        sendEntitySynth rootID "res" (realToFrac n)
    spawnActiveKnob "Noise" (0,1)  $ \n -> do
        sendEntitySynth rootID "noise-amp" (realToFrac n)
    spawnActiveKnob "EG Int" (0,2000)  $ \n -> do
        sendEntitySynth rootID "eg-int" (realToFrac n)

    spawnActiveKnob "Amp Atk" (0,1000)  $ \n -> do
        sendEntitySynth rootID "amp-attack" (realToFrac n)
    spawnActiveKnob "Amp Dec" (0,1000)  $ \n -> do
        sendEntitySynth rootID "amp-decay" (realToFrac n)
    spawnActiveKnob "Amp Sus" (0,1)  $ \n -> do
        sendEntitySynth rootID "amp-sustain" (realToFrac n)
    spawnActiveKnob "Amp Rel" (0,1000)  $ \n -> do
        sendEntitySynth rootID "amp-release" (realToFrac n)

    spawnActiveKnob "Env Atk" (0,1000)  $ \n -> do
        sendEntitySynth rootID "env-attack" (realToFrac n)
    spawnActiveKnob "Env Dec" (0,1000)  $ \n -> do
        sendEntitySynth rootID "env-decay" (realToFrac n)
    spawnActiveKnob "Env Sus" (0,1)  $ \n -> do
        sendEntitySynth rootID "env-sustain" (realToFrac n)
    spawnActiveKnob "Env Rel" (0,1000)  $ \n -> do
        sendEntitySynth rootID "env-release" (realToFrac n)

    setRepeatingAction 0.1 $ do
        degree <- randomFrom majorScale
        let note = degree + 60
        sendSynth "note" (List [realToFrac (note::Int), 100])
        let degree01 = (fromIntegral degree / 12)
            hue = degree01

        brightness <- (*0.7) <$> readKnob cutoffKnob
        noteCube   <- spawnChild $ do
            myShape       ==> Cube
            myProperties  ==> [Holographic]
            myPose        ==> position (V3 0 (0.5+degree01) 0)
            myInheritPose ==> InheritPose
            mySize        ==> 0.2
            myColor       ==> colorHSL degree01 0.8 brightness
        inEntity noteCube $ setLifetime 1

    return ()



















data KnobState = KnobState
    { ksLastHandPose :: !(M44 GLfloat)
    , ksRotation        :: !GLfloat
    }
newKnobState :: KnobState
newKnobState = KnobState identity 0

spawnActiveKnob name (low, hi) action = do
    i <- getState (0::Int)
    setState (i+1)
    let x = fromIntegral (i `div` 4) * 0.3
        y = fromIntegral (i `mod` 4) * 0.3 - 0.45
        knobPos = V3 (0.5 + x) y 0
    spawnActiveKnobAt knobPos name (low, hi) action

spawnActiveKnobAt knobPos name (low,hi) action = do
    let range = hi - low
    nameLabel <- spawnChild $ do
        myText        ==> name
        myTextPose    ==> position (V3 0 0.1 0) !*! scaleMatrix 0.05
        myInheritPose ==> InheritPose
        myPose        ==> position knobPos
    valueLabel <- spawnChild $ do
        myText        ==> show 0
        myTextPose    ==> position (V3 0 -0.1 0) !*! scaleMatrix 0.05
        myInheritPose ==> InheritPose
        myPose        ==> position knobPos

    knob <- spawnChild $ do
        myShape        ==> Cube
        mySize         ==> 0.1
        myProperties   ==> [Floating]
        myDragOverride ==> True
        myDragBegan ==> do
            withComponent_ myDragFrom $ \(DragFrom _handEntityID startM44) -> do
                editState $ \ks -> return $ ks {
                        ksLastHandPose = startM44
                    }
        myDrag ==> \dragM44 -> do
            withComponent_ myDragFrom $ \(DragFrom handEntityID _startM44) -> do
                -- Calculate the rotation for this tick
                newHandPose <- getEntityPose handEntityID
                oldState    <- getState newKnobState
                let diff = newHandPose `subtractMatrix` ksLastHandPose oldState
                    V3 dX _dY _dZ = testEpsilon $ quatToEuler (quaternionFromMatrix diff)
                    newRotation = max 0 . min (2*pi) $ ksRotation oldState + dX

                -- Update the knob's state and appearance
                setState (KnobState { ksLastHandPose = newHandPose, ksRotation = newRotation })
                setAttachmentOffset (mkTransformation
                    (axisAngle (V3 0 0 1) newRotation) knobPos)

                -- Update the knob's label,
                -- and run the its action,
                -- with the scaled value
                let newValue01 = knobRotToValue newRotation
                    newValueScaled = low + range * newValue01
                inEntity valueLabel $ setText (printf "%.2f" (newValueScaled::Float))
                action newValueScaled

    attachEntity knob
    inEntity knob $ do
        setState newKnobState
        setAttachmentOffset (position knobPos)
    return knob


knobRotToValue rot = (2*pi - rot) / (2 * pi)
readKnob knobID = knobRotToValue . ksRotation <$> inEntity knobID (getState newKnobState)

testEpsilon n = if nearZero n then 0 else n
