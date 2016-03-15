{-# LANGUAGE FlexibleContexts #-}
module BuildTree where
import Rumpus

start :: OnStart
start = do
    removeChildren
    
    rootEntityID <- ask

    --setPose (identity & translation .~ V3 0.3 1.0 0)

    let makeCelestialBody parentID radius hue = do
            cmpParent                   ==> parentID
            cmpInheritParentTransform   ==> InheritPose
            cmpSize                     ==> V3 radius radius radius
            cmpColor                    ==> hslColor hue 0.8 0.5 1
            cmpShapeType                ==> SphereShape
            cmpPhysicsProperties        ==> [NoPhysicsShape]

    let makePlanet parentID radius hue orbitRadius orbitRate = spawnEntity Transient $ do
            makeCelestialBody parentID radius hue
            cmpPose                     ==> (identity & translation .~ V3 orbitRadius 0 0)
            cmpOnUpdate                 ==> do
                n <- (orbitRate *) <$> getNow
                let x = orbitRadius * cos n
                    z = orbitRadius * sin n
                cmpPose ==> (identity & translation .~ V3 x 0 z)

    container <- spawnEntity Transient $ do
        cmpParent                   ==> rootEntityID
        cmpInheritParentTransform   ==> InheritPose
        cmpSize                     ==> V3 0.3 0.3 0.3
        cmpPhysicsProperties        ==> [NoPhysicsShape]

    scaler <- spawnEntity Transient $ do 
        cmpParent                   ==> container
        cmpInheritParentTransform   ==> InheritFull
        cmpSize                     ==> V3 1 1 1
        cmpPhysicsProperties        ==> [NoPhysicsShape]

    sun <- spawnEntity Transient $ makeCelestialBody scaler 0.1 0.5
    setEntityPose (mkTransformation (axisAngle (V3 1 0 0) 0.4) (V3 0 1 0)) sun
    sun2 <- makePlanet sun 0.03 0.68 0.4 0.1
    p1 <- makePlanet sun2 0.1 0.1 0.22 0.7
    p2 <- makePlanet sun2 0.06 0.18 0.55 0.4
    p3 <- makePlanet sun2 0.25 0.34 0.9 0.9
    m1 <- makePlanet p1 0.02 0.24 0.13 2.1
    return Nothing