module Sampler where
import Rumpus
import qualified Data.Vector as V

start :: Start
start = do
    setSynthPatch "Sampler.pd"

    let numSamples = 32 -- actual generated is 256
    children <- V.generateM numSamples $ \i -> do
        let x = fromIntegral i / 8 + 1
        spawnChild $ do
            myShape            ==> Cube
            mySize             ==> 1
            myColor            ==> V4 0.8 0.9 0.4 1
            myPose             ==> position (V3 x 0 0)
    mainID <- ask

    pose <- getPose
    button <- spawnChild $ do
        myShape           ==> Cube
        myBody            ==> Animated
        mySize            ==> 0.1
        -- Set initial pose so there's not a spurious collision
        myPose            ==> pose !*! position (V3 0 0.5 0)
        myDragOverride    ==> True
        myCollisionBegan  ==> \_ _ -> do
            hue <- randomRange (0,1)
            setColor $ colorHSL hue 0.8 0.4
            sendEntitySynth mainID "record-toggle" 1
        myCollisionEnded    ==> \_ -> do
            sendEntitySynth mainID "record-toggle" 0

    attachEntity button
    inEntity button $
        setAttachmentOffset (position $ V3 0 0.5 0)

    myUpdate          ==> do
        fftSample <- V.convert <$> readPdArray "sample-fft" 0 numSamples
        V.forM_ (V.zip children fftSample) $ \(childID, sample) -> do
            let val = sample
            inEntity childID $ do
                setSize (0.01 & _yz .~ realToFrac val * 0.1)
                myColor ==> colorHSL (realToFrac val) 0.8 0.4
        return ()
