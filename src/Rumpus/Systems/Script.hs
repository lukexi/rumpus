{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Script where
import PreludeExtra

import Rumpus.Systems.PlayPause
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Shared

tickScriptSystem :: ECSMonad ()
tickScriptSystem = whenWorldPlaying $ do
    forEntitiesWithComponent myStart $
        \(entityID, onStart) -> runEntity entityID $ do
            --putStrLnIO ("Running Start for " ++ show entityID)
            
            -- Automatically remove children when start runs.
            -- This should probably be configurable but it's what
            -- I always find myself doing so I'm hardcoding it for now.
            removeChildren

            -- Only call Start once. 
            -- Handle any exceptions therein by writing them to the error pane.
            runtimeErrors <- handleAll (\e -> return (show e)) $ do
                onStart
                return ""

            when (not (null runtimeErrors)) $ putStrLnIO runtimeErrors
            setErrorText entityID runtimeErrors
                        
            removeComponent myStart

    forEntitiesWithComponent myUpdate $
        \(entityID, update) -> do
            runEntity entityID update 
                `catchAll`
                -- FIXME display this in world somewhere...
                (\e -> putStrLnIO $ "Error in Update for entity" ++ show entityID ++ ": " ++ show e)

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


editState :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m) 
               => (a -> m a) -> m ()
editState f = withState $ \scriptState ->
    setState =<< f scriptState 

setState :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m) 
               => a -> m ()
setState scriptState = myState ==> (toDyn $! scriptState)
-- FIXME not everything is NFData, so need to figure out from API perspective how to allow non NFData (e.g. TVars) while still encouraging NFData 
--setState scriptState = myState ==> (toDyn $!! scriptState)