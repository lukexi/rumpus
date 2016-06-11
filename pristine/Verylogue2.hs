module Verylogue2 where
import Rumpus

majorScale = [0,2,4,5,7,9,11,12]

data KnobScale = Linear Float Float
               | Stepped [String]

start :: Start
start = do
    setSynthPatch "Verylogue.pd"

    rootID <- ask
    setState (0::Int)

    -- VCO1
    spawnActiveKnob "VCO1 Octave" (Stepped ["0", "1", "2", "3"]) 1 $ \n -> do
        sendEntitySynth rootID "vco1-amp" (realToFrac (floor n))
    spawnActiveKnob "VCO1 Pitch" (Linear -1 1) 0 $ \n -> do
        sendEntitySynth rootID "vco1-pitch" (realToFrac n)
    spawnActiveKnob "VCO1 Shape" (Linear 0 1) 0.5 $ \n -> do
        sendEntitySynth rootID "vco1-shape" (realToFrac n)
    spawnActiveKnob "VCO1 Wave" (Stepped ["Saw", "Sin", "Squ"]) 2 $ \n -> do
        sendEntitySynth rootID "vco1-wave" (realToFrac (floor n))

    -- VCO2
    spawnActiveKnob "VCO2 Octave" (Stepped ["0", "1", "2", "3"]) 2 $ \n -> do
        sendEntitySynth rootID "vco2-amp" (realToFrac (floor n))
    spawnActiveKnob "VCO2 Wave" (Stepped ["Saw", "Sin", "Squ"]) 1 $ \n -> do
        sendEntitySynth rootID "vco2-wave" (realToFrac (floor n))
    spawnActiveKnob "VCO2 Pitch" (Linear -1 1) 0 $ \n -> do
        sendEntitySynth rootID "vco2-pitch" (realToFrac n)
    spawnActiveKnob "VCO2 Shape" (Linear 0 1) 0.5 $ \n -> do
        sendEntitySynth rootID "vco2-shape" (realToFrac n)

    spawnActiveKnob "VCO2 Cross" (Linear 0 1) 0 $ \n -> do
        sendEntitySynth rootID "vco2-crossmod" (realToFrac n)
    spawnActiveKnob "VCO2 EG>Pitch" (Linear 0 1) 0 $ \n -> do
        sendEntitySynth rootID "vco2-pitch-eg-int" (realToFrac n)

    -- Mixer
    spawnActiveKnob "VCO1 Amp" (Linear 0 1) 1 $ \n -> do
        sendEntitySynth rootID "vco1-amp" (realToFrac n)
    spawnActiveKnob "VCO2 Amp" (Linear 0 1) 1 $ \n -> do
        sendEntitySynth rootID "vco2-amp" (realToFrac n)
    spawnActiveKnob "Noise Amp" (Linear 0 1) 0 $ \n -> do
        sendEntitySynth rootID "noise-amp" (realToFrac n)

    -- Filter
    cutoffKnob <- spawnActiveKnob "Cutoff" (Linear 0 96) 80 $ \n -> do
        sendEntitySynth rootID "cutoff" (realToFrac n)

    spawnActiveKnob "Resonance" (Linear 0 1) 0 $ \n -> do
        sendEntitySynth rootID "resonance" (realToFrac n)

    spawnActiveKnob "EG Int" (Linear 0 1) 0.5 $ \n -> do
        sendEntitySynth rootID "cutoff-eg-int" (realToFrac n)

    -- Amp EG
    spawnActiveKnob "Amp Atk" (Linear 0 1000) 10 $ \n -> do
        sendEntitySynth rootID "amp-attack" (realToFrac n)
    spawnActiveKnob "Amp Dec" (Linear 0 1000) 100 $ \n -> do
        sendEntitySynth rootID "amp-decay" (realToFrac n)
    spawnActiveKnob "Amp Sus" (Linear 0 1) 0.5 $ \n -> do
        sendEntitySynth rootID "amp-sustain" (realToFrac n)
    spawnActiveKnob "Amp Rel" (Linear 0 1000) 250 $ \n -> do
        sendEntitySynth rootID "amp-release" (realToFrac n)

    -- Assignable EG
    spawnActiveKnob "Env Atk" (Linear 0 1000) 10 $ \n -> do
        sendEntitySynth rootID "env-attack" (realToFrac n)
    spawnActiveKnob "Env Dec" (Linear 0 1000) 100 $ \n -> do
        sendEntitySynth rootID "env-decay" (realToFrac n)
    spawnActiveKnob "Env Sus" (Linear 0 1) 0.5 $ \n -> do
        sendEntitySynth rootID "env-sustain" (realToFrac n)
    spawnActiveKnob "Env Rel" (Linear 0 1000) 250 $ \n -> do
        sendEntitySynth rootID "env-release" (realToFrac n)

    -- LFO
    spawnActiveKnob "LFO Wave" (Stepped ["Saw", "Sin", "Squ"]) 0 $ \n -> do
        sendEntitySynth rootID "lfo-wave" (realToFrac (floor n))
    spawnActiveKnob "LFO EG-Mod" (Stepped ["Int", "Rate", "Off"]) 2 $ \n -> do
        sendEntitySynth rootID "lfo-eg-mod" (realToFrac (floor n))
    spawnActiveKnob "LFO Rate" (Linear 0.1 250) 7 $ \n -> do
        sendEntitySynth rootID "lfo-rate" (realToFrac n)
    spawnActiveKnob "LFO Int" (Linear 0 1) 0.1 $ \n -> do
        sendEntitySynth rootID "lfo-int" (realToFrac n)
    spawnActiveKnob "LFO Target" (Stepped ["Pitch", "Rate", "Cutoff"]) 1 $ \n -> do
        sendEntitySynth rootID "lfo-target" (realToFrac (floor n))

    -- Knob backplane
    let backW = 3.2
    spawnChild $ do
        myShape ==> Cube
        mySize  ==> V3 backW 1.5 0.01
        myPose  ==> position (V3 (0.3 + 0.5*backW) 0 -0.1)
        myColor ==> V4 0.2 0.2 0.23 1


    -- Random sequencer
    setRepeatingAction 0.1 $ do
        degree <- randomFrom majorScale
        let note = degree + 60
        sendSynth "note" (List [realToFrac (note::Int), 100])
        let degree01 = (fromIntegral degree / 12)
            hue = degree01

        brightness <- (*0.7) <$> readKnob cutoffKnob
        void . spawnChild $ do
            myShape       ==> Cube
            mySize        ==> 0.2
            myColor       ==> colorHSL degree01 0.8 brightness
            myPose        ==> position (V3 0 (0.5+degree01) 0)
            myLifetime    ==> 1

    return ()



















data KnobState = KnobState
    { ksLastHandPose :: !(M44 GLfloat)
    , ksRotation     :: !GLfloat
    }
newKnobState :: KnobState
newKnobState = KnobState identity 0

spawnActiveKnob name knobScale defVal action = do
    i <- getState (0::Int)
    setState (i+1)
    let x = fromIntegral (i `div` 4) * 0.4
        y = fromIntegral (i `mod` 4) * 0.3 - 0.45
        knobPos = V3 (0.5 + x) y 0
    spawnActiveKnobAt knobPos name knobScale defVal action

spawnActiveKnobAt knobPos name knobScale defVal action = do

    let (low, high) = case knobScale of
            Linear low high -> (low, high)
            Stepped options -> (0, fromIntegral (length options) - 1)
        range = high - low

    let val01ToValue value01 = case knobScale of
            Linear _ _ -> low + range * value01
            -- E.g. for [foo,bar,baz] 0-0.33 should be 0, 0.33-0.66 should be 1, 0.66-1 should be 2
            Stepped _  -> fromIntegral . floor . min (range + 1 - 0.001) $ (range + 1) * value01
        valueToVal01 value = (value - low) / range
        displayValue value = case knobScale of
            Linear _ _      -> (printf "%.2f" (value::Float))
            Stepped options -> let i = round value in if i >= 0 && i < length options then options !! i else "<over>"
        initialRotation = value01ToKnobRotation (valueToVal01 defVal)

    initialValue01 <- getKnobData name (valueToVal01 defVal)
    let initialValue = val01ToValue initialValue01
    action initialValue
    nameLabel <- spawnChild $ do
        myText        ==> name
        myTextPose    ==> position (V3 0 0.1 0) !*! scaleMatrix 0.05
        myPose        ==> position knobPos
    valueLabel <- spawnChild $ do
        myText        ==> displayValue initialValue
        myTextPose    ==> position (V3 0 -0.1 0) !*! scaleMatrix 0.05
        myPose        ==> position knobPos

    parentID <- ask
    knob <- spawnChild $ do
        myShape        ==> Cube
        mySize         ==> 0.1
        myBody         ==> Animated
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
                    -- We want clockwise rotation, so bound to -2pi <> 0
                    newRotation = min 0 . max (-twoPi) $ ksRotation oldState - dX

                -- Update the knob's state and appearance
                setState (KnobState { ksLastHandPose = newHandPose, ksRotation = newRotation })
                setAttachmentOffset (positionRotation knobPos
                    (axisAngle (V3 0 0 1) newRotation) )

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
        setAttachmentOffset (positionRotation knobPos (axisAngle (V3 0 0 1) initialRotation))
    return knob


knobRotationToValue01 rot = -rot / twoPi
value01ToKnobRotation val = val * (-twoPi)

twoPi = 2 * pi

readKnob knobID = knobRotationToValue01 . ksRotation <$> inEntity knobID (getState newKnobState)

testEpsilon n = if nearZero n then 0 else n
