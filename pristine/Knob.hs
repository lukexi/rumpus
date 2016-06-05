module Knob where
import Rumpus

start :: Start
start = do
    
    knob <- spawnChild $ do
        myShape           ==> Cube
        myProperties      ==> [Floating]
        mySize            ==> 0.1        
        myDragOverride ==> True
        myDrag ==> \dragM44 -> do
            
            let V3 eX eY eZ = quatToEuler (quaternionFromMatrix dragM44)
            printIO (eX, eY, eZ)

            setAttachmentOffset (mkTransformation (axisAngle (V3 0 0 1) eZ) (V3 0 1 0))
    
    attachEntity knob
    inEntity knob $ setAttachmentOffset (position $ V3 0 1 0)
            
    return ()
