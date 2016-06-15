module WeirdExceptionBug where
import Rumpus

start = do
    forM_ (take 1 $ cycle []) $ \letter -> do
        printIO letter
