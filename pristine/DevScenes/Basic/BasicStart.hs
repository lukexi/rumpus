{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: Start
start = do
    removeChildren

    rootEntityID <- ask
    let pos = V3 0 1 0
    childID <- spawnEntity $ do
        myParent           ==> rootEntityID
        myPose             ==> mkTransformation (axisAngle (V3 0 0 1) 0.3) pos
        myShape            ==> Sphere
        myProperties       ==> [Holographic]
        myInheritTransform ==> True
        mySize             ==> V3 0.5 0.6 0.6
        myColor            ==> colorHSL 0.1 0.8 0.5
        myUpdate ==> do
            now <- sin <$> getNow
            setRotation (V3 0 1 1) now

    return Nothing
