module Foo where
import Rumpus
import Graphics.GL.TextBuffer

start :: Start
start = do
    setSize (V3 0.6 0.6 0.1)
    setFloating True
    --
    
    setRepeatingAction 0.1 $ do
        n <- getNow
        setColor (hslColor n 0.5 0.5)

    setColor (hslColor 0.4 0.8 0.1)