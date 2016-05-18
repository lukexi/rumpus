module Sequencer where
import Rumpus

start :: Start
start = do
    setPdPatchFile "verylogue.pd"

    playHead <- spawnChild $ do
        myShape      ==> Cube
        myProperties ==> [Floating, Ghostly]
        mySize       ==> V3 0.01 1 1
        myUpdate ==> do
            now <- getNow
            let x = mod' now 4
            setAttachmentOffset (translateMatrix (V3 x 0 (-1)))
    attachEntity playHead

