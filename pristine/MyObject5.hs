module MyObject5 where
import Rumpus

start :: Start
start = do
    setRotation (V3 0 1 0) (pi)
    setPosition (V3 0 1 0)
    setColor (V4 0 1 0 1)
