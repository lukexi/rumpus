module Attraction where
import Rumpus

start :: Start
start = do

    xKnob        <- addKnob "CenterX" (Linear -10 10) 0
    yKnob        <- addKnob "CenterY" (Linear -10 10) 0
    zKnob        <- addKnob "CenterZ" (Linear -10 10) 0
    strengthKnob <- addKnob "Strength" (Linear 0 1) 0.1
    bodies <- forM [0..250] $ \_ -> spawnChild $ do
        myGravity ==> 0
        myLinearDamping ==> 10
        myAngularDamping ==> 10
        myBody ==> Physical
        myShape ==> Cube
        mySize  ==> 0.4

    let target = V3 0 1 0
    myUpdate ==> do
        x <- readKnob xKnob
        y <- readKnob yKnob
        z <- readKnob zKnob
        strength <- realToFrac <$> readKnob strengthKnob
        let target = V3 x y z
        forM_ bodies $ \body -> do
            position <- getEntityPosition body
            let orientation = normalize (target - position)
            applyForceToEntity body (orientation * strength)
            let V3 h s l = abs <$> orientation
            setEntityColor body (colorHSL h s l)