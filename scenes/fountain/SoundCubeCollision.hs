module SoundCubeCollision where
import Rumpus

collision :: OnCollision
collision entityID _collidedID impulse = when (impulse > 0.1) $ do
    
    sendEntityPdPatch entityID "note"    (Atom 48)
    sendEntityPdPatch entityID "trigger" (Atom (realToFrac impulse))
