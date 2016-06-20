module Room where
import Rumpus

roomSize = 400
(roomW, roomH, roomD) = (roomSize,roomSize,roomSize)
wallD = 1
shelfH = 0.15

--roomOffset = (roomH/2 - wallD/2)
roomOffset = 0

start :: Start
start = do

    let makeWall pos size hue extraProps = spawnChild $ do
            myPose       ==> position (pos & _y +~ roomOffset)
            myShape      ==> Cube
            myBody       ==> Animated
            myBodyFlags  ==> extraProps ++ [Ungrabbable]
            mySize       ==> size
            myColor      ==> colorHSL hue 0.8 0.9
            myMass       ==> 0
    makeWall (V3 0 0 (-roomD/2)) (V3 roomW roomH wallD) 0.1 [] -- back
    makeWall (V3 0 0 (roomD/2))  (V3 (roomW) roomH wallD) 0.2 [] -- front
    makeWall (V3 (-roomW/2) 0 0) (V3 wallD roomH roomD) 0.3 [] -- left
    makeWall (V3 (roomW/2)  0 0) (V3 wallD roomH roomD) 0.4 [] -- right
    makeWall (V3 0 (-roomH/2) 0) (V3 roomW wallD roomD) 0.5 [Teleportable] -- floor
    makeWall (V3 0 (roomH/2)  0) (V3 roomW wallD roomD) 0.6 [Teleportable] -- ceiling
    return ()