module VoicePillars where
import Rumpus

import qualified Data.Vector as V

start :: Start
start = do
    setSynthPatch "VoicePillars.pd"

    let numSamples = 64 -- actual generated is 256
    children <- V.generateM numSamples $ \i -> do
        let n = fromIntegral i / fromIntegral numSamples * 2 * pi
            r = 8
            x = r * cos n
            z = r * sin n
        spawnChild $ do
            myShape            ==> Cube
            mySize             ==> 1
            myTransformType    ==> AbsolutePose
            myColor            ==> V4 0.8 0.9 0.4 1
            myPose             ==> positionRotation
                                    (V3 x 0.6 z)
                                    (axisAngle (V3 0 1 0) (-n))
    mainID <- ask
    myUpdate ==> do
        fftSample <- V.convert <$> readPdArray "sample-fft" 0 numSamples
        V.forM_ (V.zip children fftSample) $ \(childID, sample) -> do
            let val = realToFrac sample
            inEntity childID $ do
                setSize (V3 1 (1 + val * 0.1) 0.2)
                myColor ==> colorHSL (val * 0.005 + 0.5) 0.8 0.4
        return ()
