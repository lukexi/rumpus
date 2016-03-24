import System.Environment
-- | A fake version of gcc that simply echos its last argument.
-- This is to replicate the --print-file-name flag of gcc
-- which is the only functionality of gcc that GHC needs.
main :: IO ()
main = do
    args <- getArgs
    if null args then return () else do
        putStrLn (last args)