{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: OnStart
start = do
    putStrLnIO "Removing children..."
    removeChildren
    putStrLnIO "Done removing children."
    
    let branch parentID n pos = do
            childID <- spawnEntity Transient $ do
                cmpParent ==> parentID
                cmpPose   ==> 
                    (newPose & posPosition .~ pos
                             & posOrientation .~ axisAngle (V3 0 0 1) 0.3)
                cmpShapeType              ==> CubeShape
                cmpPhysicsProperties      ==> [NoPhysicsShape]
                cmpInheritParentTransform ==> True
                cmpSize ==> (V3 0.5 0.6 0.5)
                cmpColor ==> hslColor (fromIntegral n/9) 0.8 0.5 1
                cmpOnUpdate ==> do
                    now <- sin <$> getNow
                    cmpPose ==> (newPose & posPosition .~ pos
                             & posOrientation .~ axisAngle (V3 0 0 1) now)
            when (n > 0) $ do
                branch childID (n - 1) (V3 1 1 0)
                branch childID (n - 1) (V3 (-1) 1 0)
    rootEntityID <- ask
    branch rootEntityID 5 0
    return Nothing
