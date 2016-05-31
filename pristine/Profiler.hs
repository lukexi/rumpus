module Profiler where
import Rumpus
import qualified Data.HashMap.Strict as Map

start :: Start
start = do
    setSize (V3 0.4 0.4 0.1)
    --setRotation (V3 0 1 0) pi

    rootSample <- getSampleHistory
    let indexedSamples = zip [0..] (Map.toList rootSample)
        numSamples = Map.size rootSample
    containerID <- spawnChild $ do
        myInheritTransform ==> InheritPose
        mySize ==> 0.3
    gauges <- inEntity containerID $ do
      Map.fromList <$> forM indexedSamples (\(i, (name, values)) -> do
        let y = i*0.1 + 1
        -- Label
        spawnChild $ do
            myPose             ==> translateMatrix (V3 0 y 0.1)
            myTextPose         ==> scaleMatrix 0.05
            myText             ==> name
            myInheritTransform ==> InheritFull
        -- Graph
        childIDs <- forM [0..maxProfilerHistory - 1] $ \z -> spawnChild $ do
            let brightness = 1 -
                    (fromIntegral z /
                    fromIntegral maxProfilerHistory
                    * 0.5 + 0.3)
            myShape            ==> Cube
            myProperties       ==> [Holographic]
            myInheritTransform ==> InheritFull
            mySize             ==> 0.1
            myColor            ==> colorHSL
                (i / fromIntegral numSamples)
                1
                brightness
            myPose             ==> translateMatrix (V3 0 y (-fromIntegral z*0.1))
        return (name, childIDs))

    --removeComponent myUpdate
    --setRepeatingAction (0.1) $ do
    removeComponent myClockAction
    myUpdate ==> do
        sampleHistory <- getSampleHistory
        forM_ (Map.toList sampleHistory) $ \(name, toList -> values) -> do
            forM_ (Map.lookup name gauges) $ \childIDs -> do
                forM_ (zip childIDs values) $ \(childID, value) -> do
                    inEntity childID $ do
                        setSize (V3 (realToFrac value * 100) 0.05 0.1)

    return ()
