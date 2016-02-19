{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: OnStart
start = do
    removeChildren
    rootEntityID <- ask
    let recorderAt y = do
            cmpPose ==> (newPose & posPosition . _y .~ y)
            cmpSize ==> 0.01
            cmpPdPatchFile ==> "scenes/sampleverse/recorder"
            cmpOnCollisionStart ==> \_ _ -> do
                sendPd "record-toggle" (Atom 1)
                hue <- liftIO randomIO
                setColor (hslColor hue 0.8 0.4 1)
            cmpOnCollisionEnd ==> \_ -> do
                sendPd "record-toggle" (Atom 0)
            cmpOnStart ==> do
                samplerEntityID <- ask
                children <- forM [0..255] $ \i -> do
                    let x = fromIntegral i / 255 * 2
                    spawnEntity Transient $ do
                        cmpSize ==> 0.1 * (realToFrac x)
                        cmpColor ==> V4 0.8 0.9 0.4 1
                        cmpPose ==> (newPose & posPosition . _x .~ x & posPosition . _y .~ y)
                        cmpPhysicsProperties ==> [NoPhysicsShape]
                        cmpParent ==> samplerEntityID
                return (Just (toDyn children))
            cmpOnUpdate ==> withScriptData (\children -> do
                fftSample <- readPdArray "sample-fft" 0 256
                forM_ (zip children fftSample) $ \(childID, sample) -> runEntity childID $ do
                    let val = sample / 255 * 40
                    cmpSize ==> (0.01 & _yz .~ realToFrac val)
                    cmpColor ==> hslColor (realToFrac val) 0.8 0.4 1
                )
                

            cmpPhysicsProperties ==> [IsKinematic]
            cmpParent ==> rootEntityID
    forM_ [1, 2] $ \y -> spawnEntity Transient $ recorderAt y
    return Nothing
