module Sampler where
import Rumpus
import qualified Data.Vector as V

start :: Start
start = do
    setSynthPatch "Sampler.pd"

    children <- V.generateM $ \i -> do
        let x = fromIntegral i / 8 + 1
        spawnChild $ do
            myShape            ==> Cube
            mySize             ==> 1
            myColor            ==> V4 0.8 0.9 0.4 1
            myPose             ==> V3 x 0 0
            myProperties       ==> [Holographic]
            myInheritPose      ==> True
    
    spawnChild $ do
        myShape           ==> Cube
        myProperties      ==> [Floating]
        myPose            ==> position (V3 0 1 0)
        mySize            ==> 0.1
        myCollisionStart  ==> \_ _ -> do
            hue <- randomRange (0,1)
            myColor ==> colorHSL hue 0.8 0.4 1
            sendSynth "record-toggle" (Atom 1)
        myCollisionEnd    ==> \_ -> do
            sendSynth "record-toggle" (Atom 0)
        
        myUpdate          ==> do
            fftSample <- readPdArray "sample-fft" 0 256
            forM_ (V.zip children fftSample) $ \(childID, sample) -> do
                let val = sample * 2
                inEntity childID $ do
                    mySize  ==> (0.1 & _yz .~ realToFrac val)
                    myColor ==> colorHSL (realToFrac val) 0.8 0.4

