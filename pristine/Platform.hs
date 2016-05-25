module Room where
import Rumpus

roomCube = 4
(roomW, roomH, roomD) = (roomCube,roomCube,roomCube)
wallD = 1

roomOffset = -wallD/2

start :: Start
start = do

    spawnChild $ do
        myPose       ==> translateMatrix (V3 0 roomOffset 0)
        myShape      ==> Cube
        myProperties ==> [Floating, Ungrabbable, Teleportable]
        mySize       ==> V3 roomW wallD roomD
        myColor      ==> colorHSL 0.1 0.8 0.6
        myMass       ==> 0
    return ()
