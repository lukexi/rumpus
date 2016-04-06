module Orrery where
import Rumpus

data Planet = Planet 
    { parent      :: Float
    , radius      :: Float
    , orbitRadius :: Float 
    , orbitRate   :: Float
    , surfaceHue  :: V4 Float
    }

start :: OnStart
start = do
    removeChildren
    rootEntityID <- ask

    let makeCelestialBody parentID radius hue = do
            cmpParent                   ==> parentID
            cmpInheritParentTransform   ==> InheritPose
            cmpSize                     ==> V3 radius radius radius
            cmpColor                    ==> hslColor hue 0.8 0.5
            cmpShapeType                ==> SphereShape
            cmpPhysicsProperties        ==> [NoPhysicsShape]

    let makePlanet Planet{..} = 
          spawnEntity Transient $ do
            makeCelestialBody parentID radius hue
            cmpPose                     ==> translateMatrix (V3 (orbitRadius*3) 0 0)
            cmpOnUpdate                 ==> do
                n <- (orbitRate *) <$> getNow
                let x = orbitRadius * cos n
                    z = orbitRadius * sin n
                setPose (identity & translation .~ V3 x 0 z)

    container <- spawnEntity Transient $ do
        cmpParent                   ==> rootEntityID
        cmpInheritParentTransform   ==> InheritPose
        cmpSize                     ==> 0.3
        cmpPhysicsProperties        ==> [NoPhysicsShape]

    scaler <- spawnEntity Transient $ do 
        cmpParent                   ==> container
        cmpInheritParentTransform   ==> InheritFull
        cmpSize                     ==> 1
        cmpPhysicsProperties        ==> [NoPhysicsShape]

    sun <- spawnEntity Transient $ do
        makeCelestialBody scaler 0.1 0.5
        cmpPose ==> mkTransformation 
                        (axisAngle (V3 1 0 0) 0.4) 
                        (V3 0 1 0)
    
    sun2 <- makePlanet $ Planet { parent = sun,  radius = 0.03,  hue = 0.68 orbitRadius = 0.4,  orbitRate = 0.1 }

    p1   <- makePlanet $ Planet { parent = sun2, radius = 0.1,   hue = 0.1  orbitRadius = 0.22, orbitRate = 0.7 }
    m1   <- makePlanet $ Planet { parent = p1,   radius = 0.02,  hue = 0.24 orbitRadius = 0.13, orbitRate = 2.1 }
    
    p2   <- makePlanet $ Planet { parent = sun2, radius = 0.06,  hue = 0.18 orbitRadius = 0.55, orbitRate = 0.4 }
    p3   <- makePlanet $ Planet { parent = sun2, radius = 0.25,  hue = 0.34 orbitRadius = 0.9,  orbitRate = 0.9 }
    
    return Nothing