module M07AddingKnobs where
import Rumpus
{-
### Adding Knobs
You can parameterize your object by adding Knobs.
To create a simple knob, call `addKnob` with a name, a range of values and an
initial value.
Knobs will appear along the sides of the code slab.
You can read the value of the knob in your myUpdate function using readKnob.
Change your start function to add speed and height knobs:
```
-}
start :: Start
start = do
    hueKnob    <- addKnob "Hues"   (Linear 0.1 1) 1
    heightKnob <- addKnob "Height" (Linear 0.1 10) 2

    forM_ [0..99] $ \i -> do
        let x = (fromIntegral (i `mod` 10) - 5) * 0.1
            z = (fromIntegral (i `div` 10) - 5) * 0.1
        spawnChild $ do
            myColor      ==> colorHSL 0.5 0.7 0.7
            myShape      ==> Sphere
            myPose       ==> position (V3 0 1 0)
            mySize       ==> 0.05
            myUpdate     ==> do
                hues   <- readKnob hueKnob
                height <- readKnob heightKnob
                t <- getNow
                let t2 = t + fromIntegral i
                setPosition (V3 x (sin t2 * 0.05 * height + 0.5) (z - 0.5))
                setColor (colorHSL (sin (t2/2) * hues) 0.7 0.7)
{-
```

Knobs are cool because their values are saved as persistent state,
which means they are also sent across in multiplayer.
-}
