module Rumpus.Systems.Script where
import PreludeExtra

import Rumpus.Systems.PlayPause
import Rumpus.Systems.Shared
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Knobs
import qualified Data.HashMap.Strict as Map




checkIfReadyToStart :: ECSMonad ()
checkIfReadyToStart = do
    startExprIDs <- Map.keys <$> getComponentMap myStartCodeFile
    haveStart <- forM startExprIDs $ \entityID -> do
        hasStart <- entityHasComponent entityID myStart
        if hasStart
            then return True
            -- If the start compilation has an error, count that as "started"
            -- anyway so we don't end up with dead scenes due to one error
            else do
                mCodeEditor <- inEntity entityID getStartCodeEditor
                return $ case mCodeEditor of
                    Just codeEditor -> codeEditor ^. cedHasResult
                    Nothing         -> True

    let allReadyToStart = and haveStart
    when allReadyToStart $
        setWorldPlaying True

tickScriptSystem :: ECSMonad ()
tickScriptSystem = do
    isWorldPlaying <- getWorldPlaying
    if isWorldPlaying
        then runScripts
        else checkIfReadyToStart

runScripts :: ECSMonad ()
runScripts = runUserScriptsWithTimeout_ $ do
    forEntitiesWithComponent myStart $
        \(entityID, onStart) -> inEntity entityID $ do
            --putStrLnIO ("Running Start for " ++ show entityID)

            -- Automatically remove children when start runs.
            -- This should probably be configurable but it's what
            -- I always find myself doing so I'm hardcoding it for now.
            -- FIXME:
            -- This has an undesirable side-effect:
            -- Children added programmatically outside myStart
            -- will get removed too. So we really only want to do this
            -- when new code is received from the CodeEditor.
            -- (e.g., when doing this:
            -- fooID <- spawnEntity $ myStart ==> animateSizeInFrom0 0.3
            -- inEntity barID $ setParent fooID
            -- bar will be immediately deleted.
            removeChildren
            removeComponent myKnobDefs

            -- Only call Start once.
            runUserFunctionProtected myStart onStart

            removeComponent myStart

    forEntitiesWithComponent myUpdate $
        \(entityID, update) -> do
            inEntity entityID $
                runUserFunctionProtected myUpdate update

withState :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m)
          => (a -> m ()) -> m ()
withState f =
    withComponent_ myState $ \dynState -> do
        case fromDynamic dynState of
            Just scriptState -> f scriptState
            Nothing -> do
                entityID <- ask
                putStrLnIO $
                    "withState: Attempted to use entityID " ++ show entityID
                        ++ "'s script data of type " ++ show dynState
                        ++ " with a function that accepts a different type."

getState :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m)
         => a -> m a
getState def = do
    dynState <- getComponent myState
    let maybeState = join (fromDynamic <$> dynState)
    return (fromMaybe def maybeState)

editState :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m)
          => (a -> m a) -> m ()
editState f = withState $ \scriptState ->
    setState =<< f scriptState

setState :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m)
               => a -> m ()
setState scriptState = myState ==> (toDyn $! scriptState)
-- FIXME not everything is NFData, so need to figure out
-- (from API perspective) how to allow
-- non NFData (e.g. TVars) while still encouraging NFData
--setState scriptState = myState ==> (toDyn $!! scriptState)
