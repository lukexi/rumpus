module Platform where
import Rumpus

-- Platform extent in x & z
w = 4
-- Platform depth in y
d = 0.5

start :: Start
start = do


    spawnChild_ $ do
        myPose       ==> position (V3 0 (-d/2 + 3) 10)
        myShape      ==> Cube
        myBody       ==> Animated
        myBodyFlags  ==> [Ungrabbable, Teleportable]
        mySize       ==> V3 w d w
        myColor      ==> colorHSL 0.67 0.5 0.3
        myStart      ==> animateSizeInFrom0 0.3
