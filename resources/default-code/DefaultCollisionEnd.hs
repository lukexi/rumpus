module DefaultCollisionEnd where
import Rumpus

collision :: OnCollisionEnd
collision entityID withEntityID = do
    setEntityColor (hslColor 0 0.8 0.4) entityID
