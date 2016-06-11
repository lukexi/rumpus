module SampleRing where
import Rumpus

import qualified Data.Vector as V

start :: Start
start = do
    setSynthPatch "VoiceRing.pd"

    let numSamples = 32 -- actual generated is 256
    children <- V.generateM numSamples $ \i -> do
        let n = fromIntegral i / fromIntegral numSamples * 2 * pi
            r = 1
            x = r * cos n
            z = r * sin n
        spawnChild $ do
            myShape            ==> Cube
            mySize             ==> 1
            myColor            ==> V4 0.8 0.9 0.4 1
            myPose             ==> positionRotation
                                    (V3 x 0.6 z)
                                    (axisAngle (V3 0 1 0) (-n))
    mainID <- ask
    myUpdate          ==> do
        fftSample <- V.convert <$> readPdArray "sample-fft" 0 numSamples
        V.forM_ (V.zip children fftSample) $ \(childID, sample) -> do
            let val = realToFrac sample * 0.05
            inEntity childID $ do
                setSize (V3 1 val 0.01)
                myColor ==> colorHSL val 0.8 0.4
        return ()
