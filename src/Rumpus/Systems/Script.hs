{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Script where
import PreludeExtra
import Rumpus.Systems.PlayPause
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Shared
import Control.Monad.Catch
--import Control.DeepSeq

tickScriptSystem :: ECSMonad ()
tickScriptSystem = whenWorldPlaying $ do
    forEntitiesWithComponent myOnStart $
        \(entityID, onStart) -> runEntity entityID $ do
            putStrLnIO ("Running OnStart for " ++ show entityID)
            -- Only call OnStart once. 
            -- Handle any exceptions therein by writing them to the error pane.
            (mScriptData, runtimeErrors) <- handleAll (\e -> return (Nothing, show e)) $ do
                mScriptData <- onStart
                -- We can't deepseq a dynamic value here, so
                -- FIXME perhaps have scripts return via a helper 
                -- function that deepseqs their script data?
                (return (mScriptData, ""))

            setErrorText entityID runtimeErrors
            
            forM_ mScriptData $ \scriptData -> 
                myScriptData ==> scriptData
            removeComponent myOnStart

    forEntitiesWithComponent myOnUpdate $
        \(entityID, onUpdate) -> do
            runEntity entityID onUpdate 
                `catchAll`
                -- FIXME display this in world somewhere...
                (\e -> putStrLnIO $ "Error in onUpdate for entity" ++ show entityID ++ ": " ++ show e)

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
editScriptData f = 
    modifyComponent myScriptData $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> toDyn <$> f scriptData
            Nothing -> ask >>= \entityID -> do
                putStrLnIO 
                    ("editScriptData: Attempted to use entityID " ++ show entityID 
                        ++ "'s script data of type " ++ show dynScriptData 
                        ++ " with a function that accepts a different type.")
                return dynScriptData

setScriptData :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m) 
               => a -> m ()
setScriptData scriptData = myScriptData ==> toDyn scriptData