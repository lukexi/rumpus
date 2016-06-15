module M05GivingLife where
import Rumpus
{-
You can give life to our entity in a variety of ways.
The swiss-army knife of vivification is the `myUpdate` component;
change your start function to:
```
-}
start :: Start
start = do
    spawnChild $ do
        myColor      ==> colorHSL 0.5 0.7 0.7
        myShape      ==> Sphere
        myPose       ==> position (V3 0 1 0)
        mySize       ==> 0.1
        myUpdate     ==> do
            t <- getNow
            setPosition (V3 0 (sin t) 0)
            setColor (colorHSL (sin (t/2)) 0.7 0.7)
{-
```
-}
