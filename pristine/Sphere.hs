module Sphere where
import Rumpus

-- Golden Section Spiral (via http://www.softimageblog.com/archives/115)
pointsOnSphere :: Int -> [V3 GLfloat]
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
    let numPoints = 30 :: Int
        sphere = pointsOnSphere numPoints
        hues = map ((/ fromIntegral numPoints) . fromIntegral) [0..numPoints]
    forM_ (zip3 [0..] sphere hues) $ \(i, pos, hue) -> spawnChild_ $ do
        myPose             ==> position (pos*0.4)
        myShape            ==> Sphere
        mySize             ==> 0.05
        myColor            ==> colorHSL hue 0.8 0.5
        myUpdate ==> do
            now <- sin . (+ fromIntegral i) <$> getNow
            setSize (realToFrac ((sin now + 1) * 0.05 + 0.01))