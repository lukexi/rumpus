module Flasher where
import Rumpus

start :: Start
start = do
    setRepeatingAction 0.1 $ do
        n <- getNow
        setColor (colorHSL n 0.5 0.5)
