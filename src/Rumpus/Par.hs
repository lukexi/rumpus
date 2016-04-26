module Rumpus.Par where
import qualified Data.Vector.Unboxed as V
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Exception
import PreludeExtra

--- Parallel matrix evaluation experiments

-- | Takes a [input] and a function from [input] to some monoid b, 
-- runs the function on chunks of [input], and glues the results back together.
parMapChunks :: (Foldable f, Monoid b, NFData b) => Int -> ([a] -> b) -> f a -> b
parMapChunks n f xs = mconcat $ parMap rdeepseq f (chunkInto n (toList xs))

-- | Naive reimplementation of parMapChunks for comparing.
-- Only spins up threads if length xs > n
naiveParMapChunks :: (Foldable f, MonadIO m, Monoid b, NFData b) => Int -> ([a] -> b) -> f a -> m b
naiveParMapChunks n f xs = do
    -- Split the input into chunks
    let chunks = chunkInto n (toList xs)
        (localChunk:threadChunks) = if null chunks then [[]] else chunks

    -- Spin up threads for chunks 1..n
    threads <- forM threadChunks (\xsForThread -> liftIO $ do
        resultVar <- newEmptyMVar
        _ <- forkIO $ do
            let !result = force (f xsForThread)  
            putMVar resultVar result
        return resultVar)

    -- Calculate chunk 0 on this thread while the other threads run
    localResults <- liftIO $ evaluate (force f localChunk) 

    -- Get thread results
    threadResults <- mapM (liftIO . takeMVar) threads

    return . mconcat $ localResults:threadResults  

chunkInto :: Int -> [t] -> [[t]]
chunkInto n l = go l
    where 
        go [] = []
        go xs = let (x,xs') = splitAt n xs
                in x:go xs' 