{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: OnStart
start = do
    removeChildren
    rootEntityID <- ask
    let recorderAt y = do
            cmpParent            ==> rootEntityID
            cmpShapeType         ==> CubeShape
            cmpPhysicsProperties ==> [IsKinematic]
            cmpPose              ==> (identity & translation . _y .~ y)
            cmpSize              ==> 0.1
            cmpPdPatchFile       ==> "scenes/sampleverse/recorder"
            cmpOnCollisionStart  ==> \_ _ -> do
                hue <- liftIO randomIO
                cmpColor ==> hslColor hue 0.8 0.4 1
                sendPd "record-toggle" (Atom 1)
            cmpOnCollisionEnd    ==> \_ -> do
                sendPd "record-toggle" (Atom 0)
            cmpOnStart           ==> do
                samplerEntityID <- ask
                children <- forM [0..255] $ \i -> do
                    let x = fromIntegral i / 8 + 1
                    spawnEntity Transient $ do
                        cmpParent                 ==> samplerEntityID
                        cmpShapeType              ==> CubeShape
                        cmpSize                   ==> 1
                        cmpColor                  ==> V4 0.8 0.9 0.4 1
                        cmpPose                   ==> (identity & translation . _x .~ x)
                        cmpPhysicsProperties      ==> [NoPhysicsShape]
                        cmpInheritParentTransform ==> True
                return (Just (toDyn children))
            cmpOnUpdate          ==> withScriptData (\children -> do
                fftSample <- readPdArray "sample-fft" 0 256
                forM_ (zip children fftSample) $ \(childID, sample) -> runEntity childID $ do
                    let val = sample * 2
                    cmpSize  ==> (0.1 & _yz .~ realToFrac val)
                    cmpColor ==> hslColor (realToFrac val) 0.8 0.4
                )
    
    forM_ [1, 2] $ \y -> spawnEntity Transient $ recorderAt y
    return Nothing
