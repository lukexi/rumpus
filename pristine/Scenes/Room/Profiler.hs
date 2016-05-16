module Profiler where
import Rumpus
import qualified Data.HashMap.Strict as Map
import System.Metrics
start :: Start
start = do


    rootSample <- getSampleHistory
    let indexedSamples = zip [0..] (Map.toList rootSample)
    gauges <- Map.fromList <$> forM indexedSamples (\(i, (name, values)) -> do
        forM_ [0..maxProfilerHistory] $ \z -> do
        childID <- spawnChild $ do
            myShape            ==> Cube
            myProperties       ==> [Holographic]
            myTextPose         ==> translateMatrix (V3 (-1) 1 0)
                                    !*! scaleMatrix 0.05
            myText             ==> name
            myInheritTransform ==> InheritPose
            myPose             ==> translateMatrix (V3 0 (i*0.05 - 0.8) (z*0.1))
        return (name, childID))

    setRepeatingAction (0.05) $ do
        sampleHistory <- getSampleHistory
        forM_ (Map.toList sampleHistory) $ \(name, history) -> do
            forM_ (Map.lookup name gauges) $ \childIDs -> do
                forM_ (zip childIDs values) $ \(childID, value) -> do
                    runEntity childID $ do
                        setSize (V3 (value * 100) 0.1 0.1)

    return ()
