module LABalls where
import Rumpus

start :: Start
start = do
    setSize (V3 0.3 0.3 0.3)

    let range = [0..4]
    let zyPositions = [(z, y, x-2) | z <- range, y <- range, x <- range]


    forM_ zyPositions $ \(z, y, x) -> do
        spawnChild $ do
            myShape      ==> Sphere
            mySize       ==> 0.5
            myProperties ==> [Floating]
            myUpdate     ==> do
                now <- (+ x) <$> getNow
                let pos = V3 (sin now*1 + x) (cos now*1 + y) (z-1.5)
                setPosition pos
            myCollisionStart ==> \otherEntityID impulse -> do
                now <- getNow
                setColor (colorHSL now 0.8 0.8)
