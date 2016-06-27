module Playhead where
import Rumpus

start :: Start
start = do
    beatsKnob    <- addKnob "Beats"    (Stepped (map show [1..64])) 16
    tempoKnob    <- addKnob "Tempo"    (Linear 80 120) 108
    heightKnob   <- addKnob "Height"   (Linear 1 4) 1

    thisID <- ask
    playHead <- spawnChild $ do
        myShape      ==> Cube
        myBodyFlags  ==> [Ungrabbable]
        myBody       ==> Detector
        mySize       ==> 0
        myUpdate     ==> do
            tempo    <- readKnob tempoKnob
            -- Move playhead
            beats    <- succ <$> readKnob beatsKnob
            height   <- readKnob heightKnob
            now      <- (* (60/tempo)) getNow
            let time = mod' now beats
                x    = time * 4 --
                size = V3 0.01 height 1

            setAttachmentOffset (position (V3 x 0 0))

            -- Scale in/out
            let timeR    = beats - time
                scaleIn  = min time  0.2 * recip 0.2
                scaleOut = min timeR 0.2 * recip 0.2
                scale    = min 1 $ scaleIn * scaleOut

            setSize (realToFrac scale * size)
            return ()
        myCollisionBegan ==> \hitEntityID _ ->
            -- Ignore hitting our own code-slab
            when (hitEntityID /= thisID) $ do
              flashColor <- getEntityColor hitEntityID
              animateColor flashColor (V4 1 1 1 1) 0.2
    attachEntity playHead (position (V3 0 0 0))

    -- track
    spawnChild $ do
        myShape ==> Cube
        myUpdate ==> do
            beats <- succ <$> readKnob beatsKnob
            setPose (position (V3 (beats / 2) 0 0))
            setSize (V3 beats 0.01 0.01)

    -- Quantize playhead position on release
    myDragEnded ==> do
        V3 x y z <- getPosition
        let quantPos = V3 (quantize x (1/4)) (quantize y (1/24)) quantize z (1/4))
        animatePositionTo quantPos 0.3

        -- Quantize to 90-degree (45? 30?) angles
        rotation <- getRotation
        let quantRot = quantRot rotation
        animateRotationTo quantRot 0.3

    return ()

quantize n quant = undefined


-- 1 GL Unit == 4 beats
-- 1 beat == 60/bpm seconds
