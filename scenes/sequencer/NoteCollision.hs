module SoundCubeCollision where
import Rumpus

collision :: OnCollision
collision entityID _collidedID _impulse = do
    
    withPdPatch entityID $ \patch -> 
        sendPd patch "trigger" (Atom 1)
