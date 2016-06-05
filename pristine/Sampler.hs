module Sampler where
import Rumpus

import qualified Data.Vector as V

start :: Start
start = do
    

    children <- V.generateM 255 $ \i -> do
        let x = fromIntegral i / 8 + 1
        spawnChild $ do
            myShape            ==> Cube
            mySize             ==> 1
            myColor            ==> V4 0.8 0.9 0.4 1
            myPose             ==> position (V3 x 0 0)
            myProperties       ==> [Holographic]
            myInheritPose      ==> InheritPose
    button <- spawnChild $ do
        myPdPatchFile ==> "Sampler.pd"
        myShape           ==> Cube
        myProperties      ==> [Floating]
        mySize            ==> 0.1
        myCollisionStart  ==> \_ _ -> do
            hue <- randomRange (0,1)
            myColor ==> colorHSL hue 0.8 0.4
            sendSynth "record-toggle" (Atom 1)
        myCollisionEnd    ==> \_ -> do
            sendSynth "record-toggle" (Atom 0)
        
        myUpdate          ==> do
            fftSample <- V.convert <$> readPdArray "sample-fft" 0 256
            V.forM_ (V.zip children fftSample) $ \(childID, sample) -> do
                let val = sample * 2
                inEntity childID $ do
                    setSize (0.1 & _yz .~ realToFrac val)
                    myColor ==> colorHSL (realToFrac val) 0.8 0.4
            return ()
    
                    
    attachEntity button
    inEntity button $ 
        setAttachmentOffset (position $ V3 0 0.5 0)
