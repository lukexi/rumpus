module Rumpus.Systems.CodeProtect where
import Rumpus.Systems.CodeEditor
import PreludeExtra
import Control.Monad.Trans.Control

import System.Timeout.Lifted
-- | We give scripts a generous 1 second
-- in case they want to generate a lot of geometry
-- SteamVR will pull the screen away for us so it won't be painful.
-- Timeout takes microseconds (1e6)
maxUserFunctionTime :: Int
maxUserFunctionTime = 1 * 10^(6::Int)

-- | Returns Just a
-- Should wrap each function call in runUserFunctionProtected
runUserScriptsWithTimeout :: MonadBaseControl IO m => m a -> m (Maybe a)
runUserScriptsWithTimeout = timeout maxUserFunctionTime

runUserScriptsWithTimeout_ :: MonadBaseControl IO m => m a -> m ()
runUserScriptsWithTimeout_ = void . runUserScriptsWithTimeout

-- | Should be called within a runUserScriptsWithTimeout
-- Handle any exceptions during a user function
-- by writing them to the error pane and clearing the function.
runUserFunctionProtected :: (MonadIO m, MonadState ECS m, MonadReader EntityID m, MonadCatch m)
                         => Key (EntityMap a) -> m () -> m ()
runUserFunctionProtected functionKey userFunction =
    userFunction `catchAll` (\e -> do
        removeComponent functionKey
        let runtimeErrors = show e
        setErrorText runtimeErrors
        putStrLnIO runtimeErrors)
