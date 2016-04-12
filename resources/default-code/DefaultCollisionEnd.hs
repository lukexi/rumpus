module DefaultCollisionEnd where
import Rumpus

collision :: CollisionEnd
collision entityID withEntityID = do
    setEntityColor (hslColor 0 0.8 0.4) entityID
