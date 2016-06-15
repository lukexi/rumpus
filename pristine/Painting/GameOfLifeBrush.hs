module Paint where
import Rumpus hiding (Array, array)
import Data.Array.Unboxed
import qualified Data.Sequence as Seq
import Data.Word
maxBlots = 1000

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

    -- Glider
    let initialGrid :: GOL
        initialGrid = listArray gridBounds (repeat 0)
                        //  [ (V2 0 0, 1)
                            , (V2 1 0, 1)
                            , (V2 2 0, 1)
                            , (V2 0 1, 1)
                            , (V2 1 2, 1)
                            ]

    removeComponent myUpdate
    initialPosition <- getPosition
    setState (initialPosition, initialGrid, Seq.empty :: Seq EntityID, 0::GLfloat)

    myDragContinues ==> \_ -> withState $ \(lastPosition, grid, blots, hue) -> do
        newPose <- getPose
        let newPosition = newPose ^. translation
        when (distance lastPosition newPosition > 0.01) $ do
            let living = filter ((> 0) . snd) (assocs grid)
            children <- forM living $ \(i, e) -> do
                let V2 x y = (fromIntegral <$> i) / (fromIntegral <$> gridMax) * 0.1
                    pos = V3 x (y+0.5) 0
                spawnChild $ do
                    myPose          ==> newPose !*! position pos
                    myShape         ==> Cube
                    mySize          ==> 0.02
                    myTransformType ==> AbsolutePose
                    myColor         ==> colorHSL
                        hue 0.7 0.8

            let (newBlots, oldBlots) = Seq.splitAt maxBlots blots
            forM_ oldBlots removeEntity
            setState
                (newPosition, tickGOL grid, Seq.fromList children <> newBlots, hue + 0.1)

