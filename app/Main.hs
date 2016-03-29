{-# LANGUAGE CPP #-}

import Rumpus.Main
#if !defined(ENABLE_LOGGING)
import Graphics.UI.GLFW.Pal
#endif
-- We structure things this way so that this 
-- main executable along with all runtime-compiled
-- code can share the same compiled Rumpus library,
-- where the actual Rumpus runtime resides.

main :: IO ()
main = do
#if !defined(ENABLE_LOGGING)
    suppressConsole "rumpus.log"
#endif
    rumpusMain
