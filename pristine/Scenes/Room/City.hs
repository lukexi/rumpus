module City where
import Rumpus

-- Golden Section Spiral 
-- (via http://www.softimageblog.com/archives/115)
pointsOnSphere (fromIntegral -> n) = 
    let inc = pi * (3 - sqrt 5)
        off = 2 / n
    in flip map [0..n] $ \k ->
        let y = k * off - 1 + (off / 2)
            r = sqrt (1 - y*y)
            phi = k * inc
        in V3 (cos phi * r) y (sin phi * r)

start :: OnStart
start = do
    removeChildren
    createBuildings
    createStars
    return Nothing

createBuildings :: EntityMonad ()
createBuildings = do
    rootEntityID <- ask
    let n = 10
        buildSites = [V3 (x*20) (-100) (z*20) | x <- [-n..n], z <- [-n..n]]
    forM_ buildSites $ \(V3 x y z) -> do
        hue <- liftIO randomIO
        
        when (x /= 0 && z /= 0) $ void . spawnEntity Transient $ do
            cmpParent                 ==> rootEntityID
            cmpPose                   ==> mkTransformation 
                                            (axisAngle (V3 0 0 1) 0) (V3 x y z)
            cmpShapeType              ==> CubeShape
            cmpPhysicsProperties      ==> [NoPhysicsShape]
            cmpSize                   ==> V3 10 100 10

            cmpColor                  ==> hslColor hue 0.8 0.8

createStars :: EntityMonad ()
createStars = do
    rootEntityID <- ask
    
    let numPoints = 300 :: Int
        sphere = pointsOnSphere numPoints
        hues = map ((/ fromIntegral numPoints) . fromIntegral) [0..numPoints]
    forM_ (zip sphere hues) $ \(pos, hue) -> void $ spawnEntity Transient $ do
        cmpParent                 ==> rootEntityID
        cmpPose                   ==> mkTransformation 
                                        (axisAngle (V3 0 0 1) 0.3) (pos * 1000)
        cmpShapeType              ==> SphereShape
        cmpPhysicsProperties      ==> [NoPhysicsShape]
        cmpInheritParentTransform ==> InheritFull
        cmpSize                   ==> 5
        cmpColor                  ==> hslColor hue 0.8 0.8