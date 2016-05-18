module Loader where
import Rumpus
-- Implements the workaround of forcing the GHC API to
-- load Rumpus before the main thread is started
-- (see CodeEditor.hs:withRumpusGHC)
start :: Start
start = return ()
