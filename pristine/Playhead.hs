module Sequencer where
import Rumpus

start :: Start
start = do
    let duration = 4
        size = V3 0.01 1 1

    thisID <- ask
    playHead <- spawnChild $ do
        myShape      ==> Cube
        myBodyFlags  ==> [Ungrabbable]
        myBody       ==> Detector
        mySize       ==> size
        myUpdate     ==> do
            now <- getNow
            let time = mod' now duration
                x = time
            setAttachmentOffset (translateMatrix (V3 x 0 0))

            -- Scale in/out
            let timeR    = duration - time
                scaleIn  = min time  0.2 * recip 0.2
                scaleOut = min timeR 0.2 * recip 0.2
                scale    = scaleIn * scaleOut
            when (scale < 1) $ do
                setSize (realToFrac scale * size)
            return ()
        myCollisionBegan ==> \hitEntityID _ -> when (hitEntityID /= thisID) $ do
            flashColor <- getEntityColor hitEntityID
            animateColor 0.2 flashColor (V4 1 1 1 1)
    attachEntity playHead (translateMatrix (V3 0 0 0))

    -- track
    spawnChild $ do
        myShape ==> Cube
        mySize  ==> V3 duration 0.01 0.01
        myPose  ==> translateMatrix (V3 (duration / 2) 0 0)

    return ()
