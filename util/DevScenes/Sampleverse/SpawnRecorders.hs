{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: Start
start = do
    removeChildren
    rootEntityID <- ask
    let recorderAt y = do
            myParent          ==> rootEntityID
            myShape           ==> Cube
            myProperties      ==> [Floating]
            myPose            ==> identity & translation . _y .~ y
            mySize            ==> 0.1
            myPdPatchFile     ==> "scenes/sampleverse/recorder"
            myCollisionStart  ==> \_ _ -> do
                hue <- liftIO randomIO
                myColor ==> colorHSL hue 0.8 0.4 1
                sendSynth "record-toggle" (Atom 1)
            myCollisionEnd    ==> \_ -> do
                sendSynth "record-toggle" (Atom 0)
            myStart           ==> do
                samplerEntityID <- ask
                children <- forM [0..255] $ \i -> do
                    let x = fromIntegral i / 8 + 1
                    spawnEntity $ do
                        myParent           ==> samplerEntityID
                        myShape            ==> Cube
                        mySize             ==> 1
                        myColor            ==> V4 0.8 0.9 0.4 1
                        myPose             ==> identity & translation . _x .~ x
                        myProperties       ==> [Holographic]
                        myInheritPose      ==> True
                return (Just (toDyn children))
            myUpdate          ==> withState (\children -> do
                fftSample <- readPdArray "sample-fft" 0 256
                forM_ (zip children fftSample) $ \(childID, sample) -> inEntity childID $ do
                    let val = sample * 2
                    mySize  ==> (0.1 & _yz .~ realToFrac val)
                    myColor ==> colorHSL (realToFrac val) 0.8 0.4
                )

    forM_ [1, 2] $ \y -> spawnEntity $ recorderAt y
    return Nothing
