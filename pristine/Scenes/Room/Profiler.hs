module Profiler where
import Rumpus
import qualified Data.HashMap.Strict as Map
import System.Metrics
start :: Start
start = do


    rootSample <- getProfilerSample
    let indexedSamples = zip [0..] (Map.toList rootSample)
    gauges <- Map.fromList <$> forM indexedSamples (\(i, (name, value)) -> do
        childID <- spawnChild $ do
            myShape            ==> Cube
            myProperties       ==> [Holographic]
            myTextPose         ==> translateMatrix (V3 0 1 0)
            myText             ==> ""
            myInheritTransform ==> InheritPose
        return (name, childID))

    setRepeatingAction (1/10) $ do
        sample <- getProfilerSample
        forM_ (Map.toList sample) $ \(name, value) -> do
            forM_ (Map.lookup name gauges) $ \childID -> do
                let text = case value of
                    Counter count -> show count
                    Gauge level -> show level
                    Label text -> show text
                    Distribution stats -> show stats
                runEntity childID $ setText text
        setText text
    return ()
