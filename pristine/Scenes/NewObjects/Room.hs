module Room where
import Rumpus

roomCube = 4
(roomW, roomH, roomD) = (roomCube,roomCube,roomCube)
wallD = 1

roomOffset = -wallD/2

start :: Start
start = do

    -- Set the pose of the code editor to be centered in the wall
    setPosition (V3 (-2.5) 1 0)
    setRotation (V3 0 1 0) (-pi/2)
    --setPosition (V3 (-1) 1 (-roomD/2 + 0.4))

    spawnChild $ do
        myPose       ==> translateMatrix (V3 0 roomOffset 0)
        myShape      ==> Cube
        myProperties ==> [Floating, Static, Teleportable]
        mySize       ==> V3 roomW wallD roomD
        myColor      ==> hslColor 0.1 0.8 0.6
        myMass       ==> 0

    spawnChild $ do
        myShape     ==> Cube
        mySize      ==> 0.3
        myColor     ==> V4 0.1 0.2 0.3 1
        myStartExpr ==> ("NewObject1.hs", "start")

    sceneFolder <- getSceneFolder

    touchFile (sceneFolder </> "NewObject2.hs")
    spawnChild $ do
        myShape     ==> Cube
        mySize      ==> 0.3
        myColor     ==> V4 0.1 0.2 0.3 1
        myStartExpr ==> ("NewObject2.hs", "start")
    return ()

-- Acts the same as "touch", creating the file if missing but not modifying an existing one
touchFile filePath = liftIO $ appendFile filePath ""
