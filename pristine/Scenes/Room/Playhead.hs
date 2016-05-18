module Sequencer where
import Rumpus

start :: Start
start = do
    setPdPatchFile "verylogue.pd"

    playHead <- spawnChild $ do
        myShape      ==> Cube
        myProperties ==> [Floating, Ghostly]
        mySize       ==> V3 1 1 0.01
        myInheritTransform ==> InheritPose
        myUpdate ==> do
            now <- getNow
            let x = mod' now 1
            setAttachmentOffset (translateMatrix (V3 x 0 (-1)))
    attachEntity playHead
    
    
    