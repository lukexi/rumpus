module M06MultipleChildren where
import Rumpus
{-

You can call spawnChild as many times as you like.
Let's create a field of dreams:
```
-}
start :: Start
start = do
    forM_ [0..99] $ \i -> do
        let x = (fromIntegral (i `mod` 10) - 5) * 0.1
            z = (fromIntegral (i `div` 10) - 5) * 0.1
        spawnChild $ do
            myColor      ==> colorHSL 0.5 0.7 0.7
            myShape      ==> Sphere
            myPose       ==> position (V3 0 1 0)
            mySize       ==> 0.05
            myUpdate     ==> do
                t <- getNow
                let t2 = t + fromIntegral i
                setPosition (V3 x (sin t2 * 0.05 + 0.5) (z - 0.5))
                setColor (colorHSL (sin (t2/2)) 0.7 0.7)
{-
```
-}