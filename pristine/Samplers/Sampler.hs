module Sampler where
import Rumpus
import qualified Data.Vector as V
import Data.String


start :: Start
start = do

    setSynthPatch "Sampler.pd"

    -- Inform the Sampler of where to save & load its sample
    mainID <- ask
    stateFolder <- getSceneStateFolder
    sampleFileName <- toPdPathStyle (stateFolder </> show mainID <.> "wav")
    sendSynth "sample-file" (fromString sampleFileName)

    addActiveKnob "Speed" (DualExponential -50 50) 1 $ \val -> do
        sendEntitySynth mainID "sample-speed" (realToFrac val)

    -- Create the entities representing the FFT
    let numSamples = 32 -- actual generated is 256
    children <- V.generateM numSamples $ \i -> do
        let x = fromIntegral i / 8 + 1
        spawnChild $ do
            myShape            ==> Cube
            mySize             ==> 1
            myColor            ==> V4 0.8 0.9 0.4 1
            myPose             ==> position (V3 x 0 0)

    -- Create a button to trigger sample recording
    pose <- getPose
    button <- spawnChild $ do
        myShape           ==> Cube
        myBody            ==> Animated
        mySize            ==> 0.1
        -- Set initial pose so there's not a spurious collision
        myPose            ==> pose !*! position (V3 0 0.5 0)
        myDragOverride    ==> True
        myDragBegan  ==> do
            hue <- randomRange (0,1)
            setColor $ colorHSL hue 0.8 0.4
            sendEntitySynth mainID "begin-recording" Bang

    attachEntity button (position $ V3 0 0.5 0)

    -- Update the FFT entities with FFT values each frame
    myUpdate ==> do
        fftSample <- V.convert <$> readPdArray "sample-fft" 0 numSamples
        V.forM_ (V.zip children fftSample) $ \(childID, sample) -> do
            let val = max 0 $ logBase 4 sample + 1
            inEntity childID $ do
                setSize (0.01 & _yz .~ realToFrac val * 0.1)
                myColor ==> colorHSL (realToFrac val / 3) 0.8 0.4
        return ()
