module SoundCubeCollision where
import Rumpus

collision :: OnCollision
collision entityID _collidedID impulse = when (impulse > 0.1) $ do
    
    sendEntityPd entityID "note"    (Atom 48)
    sendEntityPd entityID "trigger" (Atom (realToFrac impulse))
