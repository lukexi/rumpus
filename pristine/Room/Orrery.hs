module Orrery where
import Rumpus

spawnPlanetOf parentID rateKnob radius hue orbitRadius orbitRate = spawnChildOf parentID $ do
    myShape            ==> Sphere
    mySize             ==> radius
    myColor            ==> colorHSL hue 0.8 0.5
    myUpdate ==> do
        rate <- readKnob rateKnob
        now <- getNow
        let n = rate * orbitRate * now * 8
            x = orbitRadius * cos n
            z = orbitRadius * sin n
        setPosition (V3 x 0 z)

start :: Start
start = do
    -- Create a container node that inherits
    -- pose, but not scale, from the root object
    container <- spawnChild $ do
        mySize ==> 0.800

    -- Create a node that inherits scale from the container node,
    -- but whose own scale is 1
    scaler <- spawnChildOf container $ do
        myTransformType ==> RelativeFull
        mySize          ==> 1

    sun <- spawnChildOf scaler $ do
        myShape ==> Sphere
        mySize  ==> 0.08
        myColor ==> colorHSL 0.1 0.8 0.5
        myPose  ==> positionRotation
                        (V3 0 0.5 0)
                        -- Tilted axis
                        (axisAngle (V3 1 0 0) -0.4)

    rateKnob <- addKnob "Orbit Rate" (Linear 0.1 5) 1

    --                                        Radius Hue   OrbitRadius OrbitRate
    sun2    <- spawnPlanetOf sun     rateKnob 0.03   0.8   0.4         0.1
    planet0 <- spawnPlanetOf sun     rateKnob 0.02   0.0   0.1         0.7
    planet1 <- spawnPlanetOf sun2    rateKnob 0.04   0.4   0.12        0.7
    moon1   <- spawnPlanetOf planet1 rateKnob 0.01   0.8   0.03        2.1
    planet2 <- spawnPlanetOf sun2    rateKnob 0.03   0.7   0.15        0.4
    planet2 <- spawnPlanetOf sun2    rateKnob 0.03   0.55  0.2         0.5
    planet3 <- spawnPlanetOf sun2    rateKnob 0.01   0.34  0.1         0.9

    return ()
