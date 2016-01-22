module SoundCubeCollision where
import Rumpus

collision :: OnCollision
collision entityID _collidedID impulse = when (impulse > 0.1) $ do
    
    withPdPatch entityID $ \patch -> 
        sendPd patch "trigger" (Atom (realToFrac impulse))
