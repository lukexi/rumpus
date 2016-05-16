module Sequencer where
import Rumpus

start :: Start
start = do

    playHead <- spawnChild $ do
        myUpdate ==> do
            now <- getNow
            let x = mod' now 1
            setAttachmentOffset (translateMatrix (V3 x 0 (-1)))
    attachEntity playHead

    return Nothing
