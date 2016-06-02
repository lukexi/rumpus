module Seed where
import Rumpus

gestationPeriod = 3

start :: Start
start = do

    birthTime <- getNow
    myCollisionBegan ==> \_ -> do
        isHeld <- isBeingHeld
        now    <- getNow
        when (not isHeld && (now - birthTime) > gestationPeriod) $ do
            printIO "BEGIN GROWING"
    return ()
