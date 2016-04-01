{-# LANGUAGE CPP #-}

import Rumpus.Main
#if defined(RUMPUS_RELEASE)
import Graphics.UI.GLFW.Pal
#endif
-- We structure things this way so that this 
-- main executable along with all runtime-compiled
-- code can share the same compiled Rumpus library,
-- where the actual Rumpus runtime resides.

main :: IO ()
main = do
-- When building a release, route all console messages into rumpus.log
#if defined(RUMPUS_RELEASE)
    suppressConsole "rumpus.log"
#endif
    rumpusMain
