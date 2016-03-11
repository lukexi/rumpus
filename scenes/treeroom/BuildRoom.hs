{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: OnStart
start = do
    removeChildren
    builderID <- ask
    let makeWall pos size hue = spawnEntity Transient $ do
            cmpParent            ==> builderID
            cmpPose              ==> mkTransformation (axisAngle (V3 0 0 1) 0) pos
            cmpShapeType         ==> CubeShape
            cmpPhysicsProperties ==> [IsKinematic]
            cmpSize              ==> size
            cmpColor             ==> hslColor hue 0.8 0.6 1
    makeWall (V3 0    0    (-5))  (V3 10  10   0.1) 0.1 -- back
    makeWall (V3 0    0    5)     (V3 10  10   0.1) 0.2 -- front
    makeWall (V3 (-5) 0    0)     (V3 0.1 10   10)  0.3 -- left
    makeWall (V3 5    0    0)     (V3 0.1 10   10)  0.4 -- right
    makeWall (V3 0    (-5) 0)     (V3 10   0.1 10)  0.5 -- floor
    makeWall (V3 0    5    0)     (V3 10  0.1 10)   0.6 -- ceiling
    
    makeWall (V3 0    0    (-5))  (V3 10   0.1   4) 0.7 -- shelf

    return Nothing
