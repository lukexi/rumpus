module Basic where
import Rumpus

start :: Start
start = do

    spawnChild $ do
        myPose             ==> translateMatrix (V3 0 1 0)
        myShape            ==> Cube
        myProperties       ==> [Holographic]
        myInheritPose ==> True
        mySize             ==> V3 0.1 0.1 0.1
        myColor            ==> colorHSL 0.1 0.8 0.5
        myUpdate ==> do
            now <- sin <$> getNow
            setRotation (V3 0 1 1) now

    return ()
