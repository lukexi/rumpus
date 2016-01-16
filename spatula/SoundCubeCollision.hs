module SoundCubeCollision where
import Rumpus

collision :: OnCollision
collision entityID _collidedID impulse = when (impulse > 0.1) $ do
    
    withPdPatch entityID $ \patch -> do
        sendPd patch "note"    (Atom 48)
        sendPd patch "trigger" (Atom (realToFrac impulse))
