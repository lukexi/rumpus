import System.Environment
import System.Directory
-- | A fake version of gcc that simply echos its last argument.
-- This is to replicate the --print-file-name flag of gcc
-- which is the only functionality of gcc that GHC needs.
main :: IO ()
main = do
    args <- getArgs
    if null args then return () else do
        let input = last args

        -- In the case of .a files, which are newly needed as of 8.0.1,
        -- (libmingw32.a and libmingwex.a as deps of ghc-prim)
        -- we must return an absolute path as they are handled by a separate
        -- part of the GHC linker than DLLs (which are passed to Windows own
        -- search mechanisms, which include the current directory)
        -- See https://phabricator.haskell.org/D1805#69313
        putStrLn =<< case input of
            "libmingw32.a" -> makeAbsolute input
            "libmingwex.a" -> makeAbsolute input
            other          -> return other
