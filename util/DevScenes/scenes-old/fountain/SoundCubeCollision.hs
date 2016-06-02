module SoundCubeCollision where
import Rumpus

collision :: Collision
collision entityID _collidedID impulse = when (impulse > 0.1) $ do

    sendEntitySynth entityID "note"    (Atom 48)
    sendEntitySynth entityID "trigger" (Atom (realToFrac impulse))
