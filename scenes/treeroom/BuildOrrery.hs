{-# LANGUAGE FlexibleContexts #-}
module BuildTree where
import Rumpus

start :: OnStart
start = do
    removeChildren
    
    setPose (identity & translation .~ V3 0.3 1.0 0) 

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
                n <- getNow
                let x = orbitRadius * cos n
                    z = orbitRadius * sin n
                cmpPose ==> (identity & translation .~ V3 x 0 z)

    rootEntityID <- ask
    sun <- spawnEntity Transient $ makeCelestialBody rootEntityID 0.1 0.5
    setEntityPose (identity & translation .~ V3 0 1 0) sun
    planet <- makePlanet sun 0.1 0.1 0.5 0.7
    return Nothing
