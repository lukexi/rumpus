module WeirdExceptionBug where
import Rumpus
start :: Start
start = do
    --printIO "HI"
    forM_ (take 1 $ cycle []) $ \letter -> do
        printIO (letter :: Char)