module Knob where
import Rumpus

data KnobState = KnobState { ksLastHandPose :: M44 GLfloat, ksValue :: GLfloat }
newKnobState = KnobState identity 0
start :: Start
start = do

    setState newKnobState
    knob <- spawnChild $ do
        myShape        ==> Cube
        mySize         ==> 0.1
        myProperties   ==> [Floating]
        myDragOverride ==> True
        myDragBegan    ==> do
            withComponent_ myDragFrom $ \(DragFrom handEntityID startM44) -> do
                editState $ \ks -> return $ ks {
                        ksLastHandPose = startM44
                    }
        myDrag         ==> \dragM44 -> do
            withComponent_ myDragFrom $ \(DragFrom handEntityID _) -> do
                newHandPose <- getEntityPose handEntityID
                oldState    <- getState newKnobState
                let diff = newHandPose `subtractMatrix` ksLastHandPose oldState
                    V3 dX _dY _dZ = quatToEuler (quaternionFromMatrix diff)

                let newValue = ksValue oldState + dX
                setState (KnobState { ksLastHandPose = newHandPose, ksValue = newValue })

                setAttachmentOffset (mkTransformation
                    (axisAngle (V3 0 0 1) newValue) (V3 0 0.5 0))

    attachEntity knob
    inEntity knob $ setAttachmentOffset (position $ V3 0 0.5 0)

    return ()
