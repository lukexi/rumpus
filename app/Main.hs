import Rumpus.Main

-- We structure things this way so that this 
-- main executable along with all runtime-compiled
-- code can share the same compiled Rumpus library,
-- where the actual Rumpus runtime resides.

main :: IO ()
main = rumpusMain
