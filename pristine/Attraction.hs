module Attraction where
import Rumpus

start :: Start
start = do

    xKnob <- addKnob "CenterX" (Linear -10 10) 0
    yKnob <- addKnob "CenterY" (Linear -10 10) 0
    zKnob <- addKnob "CenterZ" (Linear -10 10) 0
    strengthKnob <- addKnob "Strength" (Linear 0 1) 0.1
    bodies <- forM [0..100] $ \_ -> spawnChild $ do
        myGravity ==> 0
        myBody ==> Physical
        myShape ==> Cube
        mySize ==> 0.25

    let target = V3 0 1 0

    myUpdate ==> do
        x <- getKnobValue xKnob
        y <- getKnobValue yKnob
        z <- getKnobValue zKnob
        strength <- realToFrac <$> getKnobValue strengthKnob
        let target = V3 x y z
        forM_ bodies $ \body -> do
            position <- getEntityPosition body
            let orientation = normalize (target - position)
            applyForceToEntity body (orientation * strength)
            let V3 x y z = abs <$> orientation
            inEntity body $ setColor (colorHSL x y z)
