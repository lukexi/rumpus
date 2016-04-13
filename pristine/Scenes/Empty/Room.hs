module Room where
import Rumpus

roomCube = 3
(roomW, roomH, roomD) = (roomCube,roomCube,roomCube)
wallD = 1

roomOffset = (roomH/2 - wallD/2)

start :: Start
start = do
    removeChildren

    -- Set the pose of the code editor to be centered in the wall
    setPose (identity & translation .~ V3 0 roomOffset (-roomD/2 + 0.4))

    builderID <- ask
    void . spawnEntity $ do
        myParent            ==> builderID
        myPose              ==> translateMatrix (V3 0 (-roomH/2 + roomOffset) 0)
        myShapeType         ==> CubeShape
        myPhysicsProperties ==> [Kinematic, Static, Teleportable]
        mySize              ==> V3 roomW wallD roomD
        myColor             ==> hslColor 0.1 0.8 0.6
        myMass              ==> 0
