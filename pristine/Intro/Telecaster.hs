module Telecaster where
import Rumpus

text = [ "Welcome to Rumpus"
       , "A Live-Coding Playground"
       , "For Room-Scale VR"
       ]

start :: Start
start = do

    spawnChildren_ (zip [0..] (reverse text)) $ \(i, line) -> do
        myText ==> line
        myPose ==> position (V3 0 (0.5+(fromIntegral i * 0.1)) 0)
        mySize ==> 0.05