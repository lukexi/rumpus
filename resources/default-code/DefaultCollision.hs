module DefaultCollision where
import Rumpus

collision :: Collision
collision entityID withEntityID impulse = do
    setEntityColor (hslColor impulse 0.8 0.4) entityID
