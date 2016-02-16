{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Script where
import PreludeExtra
import Data.ECS
import Rumpus.Systems.PlayPause


type OnStart            = EntityID -> ECSMonad (Maybe Dynamic)
type OnUpdate           = EntityID -> ECSMonad ()

defineComponentKey ''OnStart
defineComponentKey ''OnUpdate

defineComponentKeyWithType "ScriptData" [t|Dynamic|]

initScriptSystem :: MonadState ECS m => m ()
initScriptSystem = do
    registerComponent "OnStart"  cmpOnStart  (newComponentInterface cmpOnStart)
    registerComponent "OnUpdate" cmpOnUpdate (newComponentInterface cmpOnUpdate)


tickScriptSystem :: ECSMonad ()
tickScriptSystem = whenWorldPlaying $ do
    forEntitiesWithComponent cmpOnStart $
        \(entityID, onStart) -> do
            -- Only call OnStart once
            mScriptData <- onStart entityID
            forM_ mScriptData $ \scriptData -> 
                addComponent cmpScriptData scriptData entityID
            removeComponent cmpOnStart entityID

    forEntitiesWithComponent cmpOnUpdate $
        \(entityID, onUpdate) -> 
            onUpdate entityID

withScriptData :: (Typeable a, MonadIO m, MonadState ECS m) 
               => EntityID -> (a -> m ()) -> m ()
withScriptData entityID f = 
    withComponent entityID cmpScriptData $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> f scriptData
            Nothing -> putStrLnIO 
                ("withScriptData: Attempted to use entityID " ++ show entityID 
                    ++ "'s script data of type " ++ show dynScriptData 
                    ++ " with a function that accepts a different type.")

editScriptData :: (Typeable a, MonadIO m, MonadState ECS m) 
               => EntityID -> (a -> m a) -> m ()
editScriptData entityID f = 
    modifyComponent entityID cmpScriptData $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> toDyn <$> f scriptData
            Nothing -> do
                putStrLnIO 
                    ("editScriptData: Attempted to use entityID " ++ show entityID 
                        ++ "'s script data of type " ++ show dynScriptData 
                        ++ " with a function that accepts a different type.")
                return dynScriptData
