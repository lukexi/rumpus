{-# LANGUAGE FlexibleContexts #-}
module BuildRoom where
import Rumpus

(roomW, roomH, roomD) = (4,4,4)
wallD = 0.1

roomOffset = (roomH/2)

start :: OnStart
start = do
    removeChildren
    builderID <- ask
    let makeWall pos size hue = spawnEntity Transient $ do
            cmpParent            ==> builderID
            cmpPose              ==> mkTransformation (axisAngle (V3 0 0 1) 0) (pos & _y +~ roomOffset)
            cmpShapeType         ==> CubeShape
            cmpPhysicsProperties ==> [IsKinematic]
            cmpSize              ==> size
            cmpColor             ==> hslColor hue 0.8 0.6 1
    makeWall (V3 0          0            (-roomD/2))  (V3 roomW roomH wallD) 0.1 -- back
    makeWall (V3 0          0            (roomD/2))   (V3 roomW roomH wallD) 0.2 -- front
    makeWall (V3 (-roomW/2) 0            0)           (V3 wallD roomH roomD) 0.3 -- left
    makeWall (V3 (roomW/2)  0            0)           (V3 wallD roomH roomD) 0.4 -- right
    makeWall (V3 0          (-roomH/2)   0)           (V3 roomW wallD roomD) 0.5 -- floor
    makeWall (V3 0          (roomH/2)    0)           (V3 roomW wallD roomD) 0.6 -- ceiling
    
    makeWall (V3 0          (-roomH/5)   (roomD/2))   (V3 roomW wallD 1)     0.7 -- shelf
    makeWall (V3 0          (-roomH*2/5) (roomD/2))   (V3 roomW wallD 1)     0.7 -- shelf

    return Nothing
