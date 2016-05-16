module Profiler where
import Rumpus
import qualified Data.HashMap.Strict as Map
import System.Metrics
start :: Start
start = do


    let valueToString name value = show name ++ ": " ++ case value of
            Counter count -> show count
            Gauge level -> show level
            Label text -> show text
            Distribution stats -> show stats
    rootSample <- getProfilerSample
    let indexedSamples = zip [0..] (Map.toList rootSample)
    gauges <- Map.fromList <$> forM indexedSamples (\(i, (name, value)) -> do
        childID <- spawnChild $ do
            --myShape            ==> Cube
            myProperties       ==> [Holographic]
            myTextPose         ==> translateMatrix (V3 0 1 0)
                                    !*! scaleMatrix 0.05
            myText             ==> valueToString name value
            myInheritTransform ==> InheritPose
            myPose ==> translateMatrix (V3 0 (i*0.05 - 0.8) 0)
        return (name, childID))

    setRepeatingAction (0.05) $ do
        sample <- getProfilerSample
        forM_ (Map.toList sample) $ \(name, value) -> do
            forM_ (Map.lookup name gauges) $ \childID -> do

                runEntity childID $ setText (valueToString name value)

    return ()