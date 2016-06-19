module Telecaster where
import Rumpus

start :: Start
start = do
    let lines =
            [ "Welcome to Rumpus"
            , "A Live Coding Playground for the HTC Vive"
            , "f"
            ]

    spawnChildren_ (zip [0..] (reverse lines)) $ \(i, line) -> do
        myText ==> line
        myPose ==> position (V3 0 (1+(fromIntegral i)) 0)
        mySize ==> 0.1