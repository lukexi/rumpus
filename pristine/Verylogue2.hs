module Verylogue2 where
import Rumpus

majorScale = [0,2,4,5,7,9,11,12]

verylogueKnobs =
    [
    -- VCO1
      ( "VCO1 Octave"   , "vco1-amp"  , Stepped ["0", "1", "2", "3"], 1 )
    , ( "VCO1 Pitch"    , "vco1-pitch", Linear -1 1, 0 )
    , ( "VCO1 Shape"    , "vco1-shape", Linear 0 1, 0.5 )
    , ( "VCO1 Wave"     , "vco1-wave" , Stepped ["Saw", "Sin", "Squ"], 2 )

    -- VCO2
    , ( "VCO2 Octave"   , "vco2-amp"  , Stepped ["0", "1", "2", "3"], 2 )
    , ( "VCO2 Wave"     , "vco2-wave" , Stepped ["Saw", "Sin", "Squ"], 1 )
    , ( "VCO2 Pitch"    , "vco2-pitch", Linear -1 1, 0 )
    , ( "VCO2 Shape"    , "vco2-shape", Linear 0 1, 0.5 )

    , ( "VCO2 Cross"    , "vco2-crossmod"    , Linear 0 1, 0 )
    , ( "VCO2 EG>Pitch" , "vco2-pitch-eg-int", Linear 0 1, 0 )

    -- Mixer
    , ( "VCO1 Amp"      , "vco1-amp"     , Linear 0 1, 1 )
    , ( "VCO2 Amp"      , "vco2-amp"     , Linear 0 1, 1 )
    , ( "Noise Amp"     , "noise-amp"    , Linear 0 1, 0 )

    -- Filter
    , ( "Cutoff"        , "cutoff"       , Linear 0 96, 80 )
    , ( "Resonance"     , "resonance"    , Linear 0 1, 0 )
    , ( "EG Int"        , "cutoff-eg-int", Linear 0 1, 0.5 )

    -- Amp EG
    , ( "Amp Atk"       , "amp-attack"   , Linear 0 1000, 10 )
    , ( "Amp Dec"       , "amp-decay"    , Linear 0 1000, 100 )
    , ( "Amp Sus"       , "amp-sustain"  , Linear 0 1, 0.5 )
    , ( "Amp Rel"       , "amp-release"  , Linear 0 1000, 250 )

    -- Assignable EG
    , ( "Env Atk"       , "env-attack"   , Linear 0 1000, 10 )
    , ( "Env Dec"       , "env-decay"    , Linear 0 1000, 100 )
    , ( "Env Sus"       , "env-sustain"  , Linear 0 1, 0.5 )
    , ( "Env Rel"       , "env-release"  , Linear 0 1000, 250 )

    -- LFO
    , ( "LFO Wave"      , "lfo-wave"     , Stepped ["Saw", "Sin", "Squ"], 0 )
    , ( "LFO EG-Mod"    , "lfo-eg-mod"   , Stepped ["Int", "Rate", "Off"], 2 )
    , ( "LFO Rate"      , "lfo-rate"     , Linear 0.1 250, 7 )
    , ( "LFO Int"       , "lfo-int"      , Linear 0 1, 0.1 )
    , ( "LFO Target"    , "lfo-target"   , Stepped ["Pitch", "Rate", "Cutoff"], 1 )


    ]

start :: Start
start = do
    setSynthPatch "Verylogue.pd"

    rootID <- ask
    setState (0::Int)
    forM_ verylogueKnobs $ \(name, synthTarget, knobScale, knobDefault) ->
        spawnActiveKnob name knobScale knobDefault $ \n ->
            sendEntitySynth rootID synthTarget (realToFrac n)

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

        brightness <- (*0.7) <$> getKnobData "Cutoff" 0.7
        void . spawnChild $ do
            myShape       ==> Cube
            mySize        ==> 0.2
            myColor       ==> colorHSL degree01 0.8 brightness
            myPose        ==> position (V3 0 (0.5+degree01) 0)
            myLifetime    ==> 1

    return ()


















data KnobScale = Linear Float Float
               | Stepped [String]

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
        y = (3 - fromIntegral (i `mod` 4)) * 0.3 - 0.45
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
                    newRotation = min 0 . max maxKnobRot $ ksRotation oldState - dX

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


knobRotationToValue01 rot = rot / maxKnobRot
value01ToKnobRotation val = val * maxKnobRot

twoPi = 2 * pi
maxKnobRot = -twoPi * 3 / 4

readKnob knobID = knobRotationToValue01 . ksRotation <$> inEntity knobID (getState newKnobState)

testEpsilon n = if nearZero n then 0 else n
