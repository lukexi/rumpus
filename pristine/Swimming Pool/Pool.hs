module Pool where
import Rumpus

roomSize = 10
(roomW, roomH, roomD) = (roomSize,roomSize,roomSize)
wallD = 0.5

roomOffset = roomH/2 - wallD/2 - 10

start :: Start
start = do
    setSynthPatch "Bubbles.pd"
    let makeWall pos size hue extraProps = spawnChild $ do
            myPose       ==> position (pos & _y +~ roomOffset)
            myShape      ==> Cube
            myBody       ==> Animated
            myBodyFlags  ==> extraProps ++ [Ungrabbable]
            mySize       ==> size
            myColor      ==> colorHSL hue 0.8 0.6
            myMass       ==> 0
    let po = -roomSize/4
    makeWall (V3 po 0 (-roomD/2)) (V3 roomW roomH wallD) 0.1 [] -- back
    makeWall (V3 po 0 (roomD/2))  (V3 roomW roomH wallD) 0.2 [] -- front
    makeWall (V3 (po - roomW/2) 0 0) (V3 wallD roomH roomD) 0.3 [] -- left
    makeWall (V3 (po + roomW/2)  0 0) (V3 wallD roomH roomD) 0.4 [] -- right
    makeWall (V3 po (-roomH/2) 0) (V3 roomW wallD roomD) 0.5 [Teleportable] -- floor

    makeWall (V3 (roomW/8) (roomH/2)  0) (V3 (roomW/2) (wallD/5) (roomD/10)) 0.6 [Teleportable] -- Diving board

    setRepeatingAction 0.1 $ do
        hue <- randomRange (0.4,0.6)
        spawnChild_ $ do
            myBody     ==> Physical
            myShape    ==> Sphere
            myColor    ==> colorHSL hue 0.6 0.7
            myPose     ==> position (V3 po -5 0)
            myLifetime ==> 20
            mySize     ==> 0.01
            myStart    ==> animateSizeTo 1 1