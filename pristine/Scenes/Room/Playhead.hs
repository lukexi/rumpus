module Sequencer where
import Rumpus

start :: Start
start = do
    let duration = 4

    playHead <- spawnChild $ do
        myShape      ==> Cube
        myProperties ==> [Floating, Ghostly, Ungrabbable]
        mySize       ==> V3 0.01 1 1
        myUpdate ==> do
            now <- getNow
            let x = mod' now duration
            setAttachmentOffset (translateMatrix (V3 x 0 0))
        myCollisionStart ==> \hitEntityID _ -> do
            flashColor <- getEntityColor hitEntityID
            animateColor 0.2 flashColor (V4 1 1 1 1)
    attachEntity playHead

    -- track
    spawnChild $ do
        myShape ==> Cube
        myProperties ==> [Holographic]
        myInheritTransform ==> InheritPose
        mySize ==> V3 duration 0.01 0.01
        myPose ==> translateMatrix (V3 (duration / 2) 0 0)

    return ()
