module M06MultipleChildren where
import Rumpus
{-

You can call spawnChild as many times as you like.
Let's create a field of dreams:
```
-}
start :: Start
start = do
    forM_ [0..100] $ \i -> do
        let x = i `mod` 10
            z = i `div` 10
        spawnChild $ do
            myColor      ==> colorHSL 0.5 0.7 0.7
            myShape      ==> Sphere
            myPose       ==> position (V3 0 1 0)
            mySize       ==> 0.1
            myUpdate     ==> do
                t <- getNow
                let t2 = t + i
                setPosition (V3 x (sin t2) z)
                setColor (colorHSL (sin (t2/2)) 0.7 0.7)
{-
```
-}
