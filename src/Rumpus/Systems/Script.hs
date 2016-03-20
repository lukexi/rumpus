{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Script where
import PreludeExtra
import Rumpus.Systems.PlayPause


type OnStart  = EntityMonad (Maybe Dynamic)
type OnUpdate = EntityMonad ()

defineComponentKey ''OnStart
defineComponentKey ''OnUpdate

defineComponentKeyWithType "ScriptData" [t|Dynamic|]

initScriptSystem :: MonadState ECS m => m ()
initScriptSystem = do
    registerComponent "OnStart"  cmpOnStart  (newComponentInterface cmpOnStart)
    registerComponent "OnUpdate" cmpOnUpdate (newComponentInterface cmpOnUpdate)
    registerComponent "ScriptData" cmpScriptData (newComponentInterface cmpScriptData)

tickScriptSystem :: ECSMonad ()
tickScriptSystem = whenWorldPlaying $ do
    forEntitiesWithComponent cmpOnStart $
        \(entityID, onStart) -> runEntity entityID $ do
            printIO ("Running OnStart for " ++ show entityID)
            -- Only call OnStart once
            mScriptData <- onStart
            forM_ mScriptData $ \scriptData -> 
                cmpScriptData ==> scriptData
            removeComponent cmpOnStart

    forEntitiesWithComponent cmpOnUpdate $
        \(entityID, onUpdate) -> 
            runEntity entityID onUpdate

withScriptData :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m) 
               => (a -> m ()) -> m ()
withScriptData f = 
    withComponent_ cmpScriptData $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> f scriptData
            Nothing -> ask >>= \entityID -> putStrLnIO 
                ("withScriptData: Attempted to use entityID " ++ show entityID 
                    ++ "'s script data of type " ++ show dynScriptData 
                    ++ " with a function that accepts a different type.")

editScriptData :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m) 
               => (a -> m a) -> m ()
editScriptData f = 
    modifyComponent cmpScriptData $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> toDyn <$> f scriptData
            Nothing -> ask >>= \entityID -> do
                putStrLnIO 
                    ("editScriptData: Attempted to use entityID " ++ show entityID 
                        ++ "'s script data of type " ++ show dynScriptData 
                        ++ " with a function that accepts a different type.")
                return dynScriptData
