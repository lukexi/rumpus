module Profiler where
import Rumpus
import qualified Data.HashMap.Strict as Map
import System.Metrics
start :: Start
start = do
    setRotation (V3 0 1 0) pi

    rootSample <- getSampleHistory
    let indexedSamples = zip [0..] (Map.toList rootSample)
        numSamples = Map.size rootSample
    gauges <- Map.fromList <$> forM indexedSamples (\(i, (name, values)) -> do
        let y = i*0.1 - 1
        spawnChild $ do
                myPose ==> translateMatrix (V3 0 y 0)
                myTextPose         ==> scaleMatrix 0.05
                myText             ==> name
                myInheritTransform ==> InheritPose
        childIDs <- forM [0..maxProfilerHistory-1] $ \z -> do
            spawnChild $ do
                myShape            ==> Cube
                myProperties       ==> [Holographic]
                myInheritTransform ==> InheritPose
                mySize             ==> 0.1
                myColor            ==> colorHSL
                    (i / fromIntegral numSamples)
                    0.8
                    (fromIntegral z / fromIntegral maxProfilerHistory)
                myPose             ==> translateMatrix (V3 0 y (-fromIntegral z*0.1))
        return (name, childIDs))

    setRepeatingAction (0.01) $ do
        sampleHistory <- getSampleHistory
        forM_ (Map.toList sampleHistory) $ \(name, toList -> values) -> do
            forM_ (Map.lookup name gauges) $ \childIDs -> do
                forM_ (zip childIDs values) $ \(childID, value) -> do
                    runEntity childID $ do
                        setSize (V3 (realToFrac value * 100) 0.05 0.1)

    return ()
