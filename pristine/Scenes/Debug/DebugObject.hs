module Room where
import Rumpus

start :: Start
start = do
    setPosition (V3 0 1.1 1)
    setRotation (V3 0 1 0) (pi + 0.1)
    setColor (hslColor 0.1 0.8 0.8)

