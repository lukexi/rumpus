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
    let n = 5
        dim = 20
        height = dim * 5
        buildSites = [V3 (x * dim * 2) (-height) (z * dim * 2) | x <- [-n..n], z <- [-n..n]]
    -- Ground
    spawnEntity Transient $ do
        let y = -height*2
        cmpParent             ==> rootEntityID
        cmpPose               ==> mkTransformation 
                                      (axisAngle (V3 0 0 1) 0) (V3 0 y 0)
        cmpShapeType          ==> CubeShape
        cmpPhysicsProperties  ==> [NoPhysicsShape]
        cmpSize               ==> V3 (dim * n * 8) 1 (dim * n * 8)
        cmpColor              ==> hslColor 0.7 0.6 0.2
    forM_ buildSites $ \(V3 x y z) -> do
        hue <- liftIO randomIO
        
        when (x /= 0 && z /= 0) $ void . spawnEntity Transient $ do
            cmpParent             ==> rootEntityID
            cmpPose               ==> mkTransformation 
                                          (axisAngle (V3 0 0 1) 0) (V3 x y z)
            cmpShapeType          ==> CubeShape
            cmpPhysicsProperties  ==> [NoPhysicsShape]
            cmpSize               ==> V3 dim height dim
            cmpColor              ==> hslColor hue 0.8 0.8

createStars :: EntityMonad ()
createStars = do
    rootEntityID <- ask
    
    let numPoints = 200 :: Int
        -- Only take the upper hemisphere
        sphere = drop (numPoints `div` 2) $ pointsOnSphere numPoints
        hues = map ((/ fromIntegral numPoints) . fromIntegral) [0..numPoints]
    forM_ (zip sphere hues) $ \(pos, hue) -> void $ spawnEntity Transient $ do
        cmpParent                 ==> rootEntityID
        cmpPose                   ==> mkTransformation 
                                        (axisAngle (V3 0 0 1) 0.3) (pos * 1000)
        cmpShapeType              ==> SphereShape
        cmpPhysicsProperties      ==> [NoPhysicsShape]
        cmpSize                   ==> 5
        cmpColor                  ==> hslColor hue 0.8 0.8