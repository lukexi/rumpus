{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: OnStart
start = do
    removeChildren
    
    rootEntityID <- ask
    let pos = V3 0 1 0
    childID <- spawnEntity Transient $ do
        cmpParent ==> rootEntityID
        cmpPose   ==> 
            (newPose & posPosition .~ pos
                     & posOrientation .~ axisAngle (V3 0 0 1) 0.3)
        cmpShapeType              ==> SphereShape
        cmpPhysicsProperties      ==> [NoPhysicsShape]
        cmpInheritParentTransform ==> True
        cmpSize                   ==> V3 0.5 0.6 0.6
        cmpColor ==> hslColor 0.1 0.8 0.5
        cmpOnUpdate ==> do
            now <- sin <$> getNow
            cmpPose ==> 
                (newPose & posPosition .~ pos
                         & posOrientation .~ axisAngle (V3 0 1 1) now)

    return Nothing
