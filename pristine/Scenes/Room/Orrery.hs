module Orrery where
import Rumpus

setCelestialBody parentID radius hue = do
    myInheritTransform ==> InheritPose
    mySize             ==> radius
    myColor            ==> colorHSL hue 0.8 0.5
    myShape            ==> Sphere
    myProperties       ==> [Holographic]

spawnPlanetOf parentID radius hue orbitRadius orbitRate = spawnChildOf parentID $ do
    setCelestialBody radius hue
    myUpdate ==> do
        now <- getNow
        let n = orbitRate * now
            x = orbitRadius * cos n
            z = orbitRadius * sin n
        setPosition (V3 x 0 z)

start :: Start
start = do
    -- Create a container node that inherits
    -- pose, but not scale, from the root object
    container <- spawnChild $ do
        myInheritTransform ==> InheritPose
        mySize             ==> 0.3

    -- Create a node that inherits scale from the container node,
    -- but whose own scale is 1
    scaler <- spawnChildOf container $ do
        myInheritTransform ==> InheritFull
        mySize             ==> 1

    sun <- spawnChildOf scaler $ do
        setCelestialBody 0.1 0.5
        myPose ==> mkTransformation
                        -- Tilted axis
                        (axisAngle (V3 1 0 0) 0.4)
                        (V3 0 1 0)

    --                               Radius Hue   OrbitRadius OrbitRate
    sun2    <- spawnPlanetOf sun     0.03   0.68  0.4         0.1
    planet1 <- spawnPlanetOf sun2    0.1    0.1   0.22        0.7
    moon1   <- spawnPlanetOf planet1 0.02   0.24  0.13        2.1
    planet2 <- spawnPlanetOf sun2    0.06   0.18  0.55        0.4
    planet3 <- spawnPlanetOf sun2    0.25   0.34  0.9         0.9

    return ()
