module GOL where
import Rumpus hiding (Array, array)
import Data.Array.Unboxed
import qualified Data.Sequence as Seq
import Data.Word

type GOL = UArray (V2 Int) Word8

gridMax :: V2 Int
gridMax = 6

gridBounds :: (V2 Int, V2 Int)
gridBounds = (0, gridMax)

vmod :: V2 Int -> V2 Int -> V2 Int
V2 x1 y1 `vmod` V2 x2 y2 = V2 (x1 `mod` x2) (y1 `mod` y2)

stencil :: [V2 Int]
stencil = [ V2 x y | x <- [-1..1], y <- [-1..1], not (x==0 && y==0) ]

tickGOL :: GOL -> GOL
tickGOL grid = array (bounds grid) (go <$> assocs grid)
  where
    go (i, e) =
        let neighbors = foldl' (\n s ->
                let j = (i + s) `vmod` gridMax
                in n + (grid ! j))
                (0::Word8) stencil
            _ = neighbors :: Word8
            checkCell 0 | neighbors == 3 = 1
            checkCell 1 | neighbors <= 1 = 0
            checkCell 1 | neighbors >= 4 = 0
            checkCell n                  = n
        in (i, checkCell e)

start :: Start
start = do

    let initialGrid :: GOL
        initialGrid = listArray gridBounds (repeat 0)
                        //  [ (V2 0 0, 1)
                            , (V2 1 0, 1)
                            , (V2 2 0, 1)
                            , (V2 0 1, 1)
                            , (V2 1 2, 1)
                            ]

    setState initialGrid
    myUpdate ==> do
        withState $ \grid -> do
            removeChildren
            forM_ (assocs grid) $ \(i, e) -> do
                let V2 x y = (fromIntegral <$> i) / (fromIntegral <$> gridMax) * 0.1
                    pos = V3 x (y+0.5) 0
                spawnChild $ do
                    myPose       ==> position pos
                    myShape      ==> Cube
                    mySize       ==> 0.01
                    myProperties ==> [Holographic]
                    myInheritTransform ==> InheritPose
                    myColor      ==> colorHSL
                        (fromIntegral e/2) 0.7 (fromIntegral e)
            setState (tickGOL grid)

