module Tiles where
import Rumpus

-- Platform extent in x & z
w = 4
-- Platform depth in y
d = 0.5
-- Number of platforms in each direction
n = 5::Int
ns = [-n..n]

size  = V3 w d w

tileCoords = [V2 x z | x <- ns, z <- ns]

start :: Start
start = do
    forM_ (zip [0..] tileCoords) $ \(i, intCoord) -> do
        let V2 xI zI = intCoord
            V2 x z = fromIntegral <$> intCoord 
        let pos   = V3 (x*4) (y-d/2) (z*4)
            -- Raise the platforms around the rim
            y     = if -n `elem` [xI,zI] || n `elem` [xI,zI] then 1 else 0
            -- Checkerboard pattern
            color = if i `mod` 2 == 0 then V4 0.1 0.1 0.1 1 else V4 1 1 1 1
        spawnChild $ do
            myPose       ==> position pos
            myShape      ==> Cube
            myBody       ==> Animated
            myBodyFlags  ==> [Ungrabbable, Teleportable]
            mySize       ==> size
            myColor      ==> color
    return ()