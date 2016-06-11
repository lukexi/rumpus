module Knob where
import Rumpus
import Text.Printf

majorScale = [0,2,4,5,7,9,11,12]

start :: Start
start = do
    setSynthPatch "Verylogue.pd"

    rootID <- ask
    setState (0::Int)

    -- VCO1
    spawnActiveKnob "VCO1 Octave" (0,4.99) 1 $ \n -> do
        sendEntitySynth rootID "vco1-amp" (realToFrac (floor n))
    spawnActiveKnob "VCO1 Pitch" (-1,1) 0 $ \n -> do
        sendEntitySynth rootID "vco1-pitch" (realToFrac n)
    spawnActiveKnob "VCO1 Shape" (0,1) 0.5 $ \n -> do
        sendEntitySynth rootID "vco1-shape" (realToFrac n)
    spawnActiveKnob "VCO1 Wave" (0,2.99) 2 $ \n -> do
        sendEntitySynth rootID "vco1-wave" (realToFrac (floor n))

    -- VCO2
    spawnActiveKnob "VCO2 Octave" (0,4.99) 2 $ \n -> do
        sendEntitySynth rootID "vco2-amp" (realToFrac (floor n))
    spawnActiveKnob "VCO2 Wave" (0,2.99) 1 $ \n -> do
        sendEntitySynth rootID "vco2-wave" (realToFrac (floor n))
    spawnActiveKnob "VCO2 Pitch" (-1,1) 0 $ \n -> do
        sendEntitySynth rootID "vco2-pitch" (realToFrac n)
    spawnActiveKnob "VCO2 Shape" (0,1) 0.5 $ \n -> do
        sendEntitySynth rootID "vco2-shape" (realToFrac n)

    spawnActiveKnob "VCO2 Cross" (0,1) 0 $ \n -> do
        sendEntitySynth rootID "vco2-crossmod" (realToFrac n)
    spawnActiveKnob "VCO2 EG>Pitch" (0,1) 0 $ \n -> do
        sendEntitySynth rootID "vco2-pitch-eg-int" (realToFrac n)

    -- Mixer
    spawnActiveKnob "VCO1 Amp" (0,1) 1 $ \n -> do
        sendEntitySynth rootID "vco1-amp" (realToFrac n)
    spawnActiveKnob "VCO2 Amp" (0,1) 1 $ \n -> do
        sendEntitySynth rootID "vco2-amp" (realToFrac n)
    spawnActiveKnob "Noise Amp" (0,1) 0 $ \n -> do
        sendEntitySynth rootID "noise-amp" (realToFrac n)

    cutoffKnob <- spawnActiveKnob "Cutoff" (0, 96) 80 $ \n -> do
        sendEntitySynth rootID "cutoff" (realToFrac n)

    spawnActiveKnob "Resonance" (0,1) 0 $ \n -> do
        sendEntitySynth rootID "res" (realToFrac n)

    spawnActiveKnob "EG Int" (0,1) 0.5 $ \n -> do
        sendEntitySynth rootID "cutoff-eg-int" (realToFrac n)

    spawnActiveKnob "Amp Atk" (0,1000) 10 $ \n -> do
        sendEntitySynth rootID "amp-attack" (realToFrac n)
    spawnActiveKnob "Amp Dec" (0,1000) 100 $ \n -> do
        sendEntitySynth rootID "amp-decay" (realToFrac n)
    spawnActiveKnob "Amp Sus" (0,1) 0.5 $ \n -> do
        sendEntitySynth rootID "amp-sustain" (realToFrac n)
    spawnActiveKnob "Amp Rel" (0,1000) 250 $ \n -> do
        sendEntitySynth rootID "amp-release" (realToFrac n)

    spawnActiveKnob "Env Atk" (0,1000) 10 $ \n -> do
        sendEntitySynth rootID "env-attack" (realToFrac n)
    spawnActiveKnob "Env Dec" (0,1000) 100 $ \n -> do
        sendEntitySynth rootID "env-decay" (realToFrac n)
    spawnActiveKnob "Env Sus" (0,1) 0.5 $ \n -> do
        sendEntitySynth rootID "env-sustain" (realToFrac n)
    spawnActiveKnob "Env Rel" (0,1000) 250 $ \n -> do
        sendEntitySynth rootID "env-release" (realToFrac n)

    -- LFO
    spawnActiveKnob "LFO Wave" (0,2.99) 0 $ \n -> do
        sendEntitySynth rootID "lfo-wave" (realToFrac (floor n))
    spawnActiveKnob "LFO EG-Mod" (0,2.99) 2 $ \n -> do
        sendEntitySynth rootID "lfo-eg-mod" (realToFrac (floor n))
    spawnActiveKnob "LFO Rate" (0.1,250) 7 $ \n -> do
        sendEntitySynth rootID "lfo-rate" (realToFrac n)
    spawnActiveKnob "LFO Int" (0,1) 0.1 $ \n -> do
        sendEntitySynth rootID "lfo-int" (realToFrac n)
    spawnActiveKnob "LFO Target" (0,2.99) 1 $ \n -> do
        sendEntitySynth rootID "lfo-target" (realToFrac (floor n))

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

    let backW = 3.2
    spawnChild $ do
        myShape ==> Cube
        myProperties ==> [Holographic]
        myInheritPose ==> InheritPose
        myPose ==> position (V3 (0.3 + 0.5*backW) 0 -0.1)
        mySize ==> V3 backW 1.5 0.01
        myColor ==> V4 0.2 0.2 0.23 1

    return ()



















data KnobState = KnobState
    { ksLastHandPose :: !(M44 GLfloat)
    , ksRotation     :: !GLfloat
    }
newKnobState :: KnobState
newKnobState = KnobState identity 0

spawnActiveKnob name (low, hi) defVal action = do
    i <- getState (0::Int)
    setState (i+1)
    let x = fromIntegral (i `div` 4) * 0.4
        y = fromIntegral (i `mod` 4) * 0.3 - 0.45
        knobPos = V3 (0.5 + x) y 0
    spawnActiveKnobAt knobPos name (low, hi) defVal action

spawnActiveKnobAt knobPos name (low,hi) defVal action = do
    let range = hi - low
        val01ToValue value01 = low + range * value01
        valueToVal01 value = (value - low) / range
        displayValue value = (printf "%.2f" (value::Float))
        initialRotation = value01ToKnobRotation (valueToVal01 defVal)

    initialValue01 <- getKnobData name (valueToVal01 defVal)
    let initialValue = val01ToValue initialValue01
    action initialValue
    nameLabel <- spawnChild $ do
        myText        ==> name
        myTextPose    ==> position (V3 0 0.1 0) !*! scaleMatrix 0.05
        myInheritPose ==> InheritPose
        myPose        ==> position knobPos
    valueLabel <- spawnChild $ do
        myText        ==> displayValue initialValue
        myTextPose    ==> position (V3 0 -0.1 0) !*! scaleMatrix 0.05
        myInheritPose ==> InheritPose
        myPose        ==> position knobPos

    parentID <- ask
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
                    newRotation = max 0 . min twoPi $ ksRotation oldState + dX

                -- Update the knob's state and appearance
                setState (KnobState { ksLastHandPose = newHandPose, ksRotation = newRotation })
                setAttachmentOffset (mkTransformation
                    (axisAngle (V3 0 0 1) newRotation) knobPos)

                -- Update the knob's label,
                -- and run the its action,
                -- with the scaled value
                let newValue01 = knobRotationToValue01 newRotation
                    newValueScaled = val01ToValue newValue01
                inEntity valueLabel $ setText (displayValue newValueScaled)
                action newValueScaled

                -- Record the knob value in the parent so it can be persisted
                inEntity parentID $ do
                    setKnobData name newValue01
        myDragEnded ==> do
            sceneWatcherSaveEntity parentID
    attachEntity knob
    inEntity knob $ do
        setState newKnobState { ksRotation = initialRotation }
        setAttachmentOffset (mkTransformation (axisAngle (V3 0 0 1) initialRotation) knobPos)
    return knob


knobRotationToValue01 rot = (twoPi - rot) / twoPi
value01ToKnobRotation val = val * twoPi

twoPi = 2 * pi

readKnob knobID = knobRotationToValue01 . ksRotation <$> inEntity knobID (getState newKnobState)

testEpsilon n = if nearZero n then 0 else n
