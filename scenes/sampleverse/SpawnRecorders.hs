{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: OnStart
start = do
    rootEntityID <- ask
    let recorderAt x = do
            cmpPose ==> newPose & posPosition . _x .~ x
            cmpSize ==> 0.25
            cmpPdPatchFile ==> "scenes/sampleverse/recorder"
            cmpOnCollisionStart ==> \_ _ -> do
                sendPd "record-toggle" (Atom 1)
                hue <- liftIO randomIO
                setColor (hslColor hue 0.8 0.4 1)
            cmpOnCollisionEnd ==> \_ -> do
                sendPd "record-toggle" (Atom 0)
            cmpOnStart ==> do
                children <- forM [0..512] $ \i -> do
                    let x = fromIntegral i / 512 * 0.2
                    spawnEntity Transient $ do
                        cmpSize ==> 0.1 * (realToFrac x)
                        cmpColor ==> V4 0.8 0.9 0.4 1
                        cmpPose ==> (newPose & posPosition . _x .~ x)
                        cmpPhysicsProperties ==> [NoPhysicsShape]
                return (Just (toDyn children))
            cmpOnUpdate ==> (withScriptData $ \children -> do

                fftSample <- readPdArray "sample-fft" 0 512 -- should localize the array name
                forM_ (zip children fftSample) $ \(childID, val) -> do
                    runEntity childID $ (cmpSize ==> realToFrac val)
                )

            cmpPhysicsProperties ==> [IsKinematic]
            cmpParent ==> rootEntityID
    forM_ [-0.75, 0.5] $ \x -> spawnEntity Transient $ recorderAt x
    return Nothing
