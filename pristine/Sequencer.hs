module Sequencer where
import Rumpus

start :: Start
start = do
    durationKnob <- addKnob "Duration" (Stepped (map show [1..10])) 3
    heightKnob <- addKnob "Height" (Linear 1 4) 1
    
    thisID <- ask
    playHead <- spawnChild $ do
        myShape      ==> Cube
        myBodyFlags  ==> [Ungrabbable]
        myBody       ==> Detector
        mySize       ==> 0
        myUpdate     ==> do
            duration <- succ <$> getKnobValue durationKnob
            height <- getKnobValue heightKnob
            let size = V3 0.01 height 1
            now <- getNow
            let time = mod' now duration
                x = time
            setAttachmentOffset (position (V3 x 0 0))

            -- Scale in/out
            let timeR    = duration - time
                scaleIn  = min time  0.2 * recip 0.2
                scaleOut = min timeR 0.2 * recip 0.2
                scale    = min 1 $ scaleIn * scaleOut
            
            setSize (realToFrac scale * size)
            return ()
        myCollisionBegan ==> \hitEntityID _ -> 
            when (hitEntityID /= thisID) $ do
              flashColor <- getEntityColor hitEntityID
              animateColor 0.2 flashColor (V4 1 1 1 1)
    attachEntity playHead (position (V3 0 0 0))

    -- track
    spawnChild $ do
        myShape ==> Cube
        myUpdate ==> do
            duration <- succ <$> getKnobValue durationKnob
            setPose (position (V3 (duration / 2) 0 0))
            setSize (V3 duration 0.01 0.01) 

    return ()