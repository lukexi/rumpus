module Orrery where
import Rumpus

data Planet = Planet 
    { parent      :: EntityID
    , radius      :: Float
    , orbitRadius :: Float 
    , orbitRate   :: Float
    , surfaceHue  :: Float
    }

start :: Start
start = do
    removeChildren
    rootEntityID <- ask

    let makeCelestialBody parentID radius hue = do
            myParent                   ==> parentID
            myInheritTransform   ==> InheritPose
            mySize                     ==> V3 radius radius radius
            myColor                    ==> hslColor hue 0.8 0.5
            myShape                ==> Sphere
            myProperties        ==> [NoPhysicsShape]

    let makePlanet Planet{..} = spawnEntity $ do

            makeCelestialBody parent radius surfaceHue
            myPose                     ==> translateMatrix (V3 (orbitRadius*3) 0 0)
            myUpdate                 ==> do
                n <- (orbitRate *) <$> getNow
                let x = orbitRadius * cos n
                    z = orbitRadius * sin n
                setPose (identity & translation .~ V3 x 0 z)

    -- Create a container node that inherits pose, but not scale, from the root object
    container <- spawnEntity $ do
        myParent                   ==> rootEntityID
        myInheritTransform   ==> InheritPose
        mySize                     ==> 0.3

    -- Create a node that inherits scale from the container node, but whose own scale is 1
    -- (FIXME: this is probably more complicated than it has to be)
    scaler <- spawnEntity $ do 
        myParent                   ==> container
        myInheritTransform   ==> InheritFull
        mySize                     ==> 1

    sun <- spawnEntity $ do
        makeCelestialBody scaler 0.1 0.5
        myPose ==> mkTransformation 
                        (axisAngle (V3 1 0 0) 0.4) 
                        (V3 0 1 0)
    
    sun2 <- makePlanet $ Planet { parent = sun,  radius = 0.03,  surfaceHue = 0.68, orbitRadius = 0.4,  orbitRate = 0.1 }

    p1   <- makePlanet $ Planet { parent = sun2, radius = 0.1,   surfaceHue = 0.1 , orbitRadius = 0.22, orbitRate = 0.7 }
    m1   <- makePlanet $ Planet { parent = p1,   radius = 0.02,  surfaceHue = 0.24, orbitRadius = 0.13, orbitRate = 2.1 }
    
    p2   <- makePlanet $ Planet { parent = sun2, radius = 0.06,  surfaceHue = 0.18, orbitRadius = 0.55, orbitRate = 0.4 }
    p3   <- makePlanet $ Planet { parent = sun2, radius = 0.25,  surfaceHue = 0.34, orbitRadius = 0.9,  orbitRate = 0.9 }
    
    return ()