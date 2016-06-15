module Verylogue2 where
import Rumpus

majorScale = [0,2,4,5,7,9,11,12]

start :: Start
start = do
    setSynthPatch "Verylogue.pd"

    rootID <- ask
    forM_ verylogueKnobs $ \(name, synthTarget, knobScale, knobDefault) ->
        addActiveKnob name knobScale knobDefault $ \n ->
            sendEntitySynth rootID synthTarget (realToFrac n)

    -- Knob backplane
    let backW = 3.2
    spawnChild $ do
        myShape ==> Cube
        mySize  ==> V3 backW 1.5 0.01
        myPose  ==> position (V3 (0.3 + 0.5*backW) 0 -0.1)
        myColor ==> V4 0.2 0.2 0.23 1



    -- Random sequencer
    setRepeatingAction 0.5 $ do
        degree <- randomFrom majorScale
        let note = degree + 60
        sendSynth "note" (List [realToFrac (note::Int), 100])
        let degree01 = (fromIntegral degree / 12)
            hue = degree01

        brightness <- (*0.7) <$> getKnobValue01ByName "Cutoff"
        -- Spawn cubes as absolute so they hang in the air
        currentPose <- getPose
        spawnChild_ $ do
            myShape         ==> Cube
            mySize          ==> 0.2
            myColor         ==> colorHSL degree01 0.8 brightness
            myPose          ==> currentPose !*! position (V3 0 (0.5+degree01) 0)
            myTransformType ==> AbsolutePose
            myLifetime      ==> 1

    return ()

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
    , ( "Amp Atk"       , "amp-attack"   , Linear 1 1000, 10 )
    , ( "Amp Dec"       , "amp-decay"    , Linear 1 1000, 100 )
    , ( "Amp Sus"       , "amp-sustain"  , Linear 0 1, 0.5 )
    , ( "Amp Rel"       , "amp-release"  , Linear 1 4000, 1000 )

    -- Assignable EG
    , ( "Env Atk"       , "env-attack"   , Linear 1 1000, 10 )
    , ( "Env Dec"       , "env-decay"    , Linear 1 1000, 100 )
    , ( "Env Sus"       , "env-sustain"  , Linear 0 1, 0.5 )
    , ( "Env Rel"       , "env-release"  , Linear 1 4000, 1000 )

    -- LFO
    , ( "LFO Wave"      , "lfo-wave"     , Stepped ["Saw", "Sin", "Squ"], 0 )
    , ( "LFO EG-Mod"    , "lfo-eg-mod"   , Stepped ["Int", "Rate", "Off"], 2 )
    , ( "LFO Rate"      , "lfo-rate"     , Linear 0.1 250, 7 )
    , ( "LFO Int"       , "lfo-int"      , Linear 0 1, 0.1 )
    , ( "LFO Target"    , "lfo-target"   , Stepped ["Pitch", "Rate", "Cutoff"], 1 )


    ]
