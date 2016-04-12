{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Script where
import PreludeExtra
import Rumpus.Systems.PlayPause
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Shared
import Control.Monad.Catch
import Control.DeepSeq

tickScriptSystem :: ECSMonad ()
tickScriptSystem = whenWorldPlaying $ do
    forEntitiesWithComponent myStart $
        \(entityID, onStart) -> runEntity entityID $ do
            putStrLnIO ("Running Start for " ++ show entityID)
            -- Only call Start once. 
            -- Handle any exceptions therein by writing them to the error pane.
            runtimeErrors <- handleAll (\e -> return (show e)) $ do
                onStart
                return ""

            setErrorText entityID runtimeErrors
                        
            removeComponent myStart

    forEntitiesWithComponent myUpdate $
        \(entityID, update) -> do
            runEntity entityID update 
                `catchAll`
                -- FIXME display this in world somewhere...
                (\e -> putStrLnIO $ "Error in Update for entity" ++ show entityID ++ ": " ++ show e)

withScriptData :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m) 
               => (a -> m ()) -> m ()
withScriptData f = 
    withComponent_ myScriptData $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> f scriptData
            Nothing -> ask >>= \entityID -> putStrLnIO 
                ("withScriptData: Attempted to use entityID " ++ show entityID 
                    ++ "'s script data of type " ++ show dynScriptData 
                    ++ " with a function that accepts a different type.")

editScriptData :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m) 
               => (a -> m a) -> m ()
editScriptData f = withScriptData $ \scriptData ->
    setScriptData =<< f scriptData 

setScriptData :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m) 
               => a -> m ()
setScriptData scriptData = myScriptData ==> toDyn scriptData
-- FIXME not everything is NFData, so need to figure out from API perspective how to allow non NFData (e.g. TVars) while still encouraging NFData 
--setScriptData scriptData = myScriptData ==> (toDyn $!! scriptData)