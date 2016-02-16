module DefaultCollision where
import Rumpus

collision :: OnCollision
collision entityID withEntityID impulse = do
    setEntityColor (hslColor impulse 0.8 0.4 1) entityID
