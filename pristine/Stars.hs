module Stars where
import Rumpus

-- Golden Section Spiral
-- (via http://www.softimageblog.com/archives/115)
pointsOnSphere (fromIntegral -> n) =
    map (\k ->
        let y = k * off - 1 + (off / 2)
            r = sqrt (1 - y*y)
            phi = k * inc
        in V3 (cos phi * r) y (sin phi * r)
        ) [0..n]
    where inc = pi * (3 - sqrt 5)
          off = 2 / n


start :: Start
start = do

    let numPoints = 200 :: Int
        -- Only take the upper hemisphere
        sphere = drop (numPoints `div` 2) $ pointsOnSphere numPoints
        hues = map ((/ fromIntegral numPoints) . fromIntegral) [0..numPoints]
    forM_ (zip sphere hues) $ \(pos, hue) -> void . spawnChild $ do
        myShape      ==> Sphere
        mySize       ==> 5
        myColor      ==> colorHSL hue 0.8 0.8
        myPose       ==> position (pos * 1000)
