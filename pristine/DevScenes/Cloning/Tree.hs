{-# LANGUAGE FlexibleContexts #-}
module Tree where
import Rumpus

start :: OnStart
start = do
    removeChildren
    
    let branch parentID n pos = do
            childID <- spawnEntity $ do
                myParent ==> parentID
                myPose   ==> mkTransformation 
                    (axisAngle (V3 0 0 1) 0.3) pos
                myShapeType              ==> SphereShape
                myPhysicsProperties      ==> [NoPhysicsShape]
                myInheritParentTransform ==> InheritFull
                mySize                   ==> V3 0.5 0.5 0.5
                myColor ==> hslColor (fromIntegral n/9) 0.8 0.5
                myOnUpdate ==> do
                    now <- sin <$> getNow
                    let V3 pX pY pZ = pos
                    myPose ==> mkTransformation 
                        (axisAngle (V3 0 1 1) (now*2)) 
                        (V3 (pX+now) pY pZ)
            when (n > 0) $ do
                branch childID (n - 1) (V3 1 1 0)
                branch childID (n - 1) (V3 (-1) 1 0)
    rootEntityID <- ask
    branch rootEntityID (3::Int) (V3 0 1 0)
    return Nothing