module Profiler where
import Rumpus

start :: Start
start = do

    childID <- spawnChild $ do
        myTextPose    ==> translateMatrix (V3 0 1 0)
        myText        ==> ""
        myInheritTransform ==> InheritPose
    runEntity childID $ do
        
        setRepeatingAction (1/10) $ do
            sample <- getProfilerSample
            setText (show sample)
    return ()