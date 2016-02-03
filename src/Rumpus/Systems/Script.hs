{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Script where
import PreludeExtra

import qualified Data.Map as Map

import Rumpus.Types
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Shared

scriptingSystem :: WorldMonad ()
scriptingSystem = do
    traverseM_ (Map.toList <$> use (wldComponents . cmpOnStart)) $ 
        \(entityID, onStart) -> do
            scriptData <- onStart entityID
            wldComponents . cmpScriptData . at entityID .= scriptData
            -- Only call OnStart once
            wldComponents . cmpOnStart . at entityID .= Nothing

    traverseM_ (Map.toList <$> use (wldComponents . cmpOnUpdate)) $ 
        \(entityID, onUpdate) -> 
            onUpdate entityID



addScriptComponent :: (MonadReader WorldStatic m, MonadState World m, MonadIO m) => EntityID -> Entity -> m ()
addScriptComponent entityID entity = do

    forM_ (entity ^. entOnStart) $ \scriptPath -> do

        let codeExprKey = (scriptPath, "start")
        _ <- createCodeEditor codeExprKey
        
        wldComponents . cmpOnStartExpr . at entityID ?= codeExprKey

    forM_ (entity ^. entOnUpdate) $ \scriptPath -> do
        let codeExprKey = (scriptPath, "update")
        _ <- createCodeEditor codeExprKey
        
        wldComponents . cmpOnUpdateExpr . at entityID ?= codeExprKey

    forM_ (entity ^. entOnCollision) $ \scriptPath -> do
        let codeExprKey = (scriptPath, "collision")
        _ <- createCodeEditor codeExprKey
        
        wldComponents . cmpOnCollisionExpr . at entityID ?= codeExprKey

removeScriptComponent :: MonadState World m => EntityID -> m ()
removeScriptComponent entityID = do
    wldComponents . cmpOnStartExpr . at entityID .= Nothing
    wldComponents . cmpOnUpdateExpr . at entityID .= Nothing
    wldComponents . cmpOnCollisionExpr . at entityID .= Nothing
    wldComponents . cmpOnStart . at entityID .= Nothing
    wldComponents . cmpOnUpdate . at entityID .= Nothing
    wldComponents . cmpOnCollision . at entityID .= Nothing
    wldComponents . cmpScriptData . at entityID .= Nothing



withScriptData :: (Typeable a, MonadIO m, MonadState World m) =>
                    EntityID -> (a -> m ()) -> m ()
withScriptData entityID f = 
    traverseM_ (use (wldComponents . cmpScriptData . at entityID)) $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> f scriptData
            Nothing -> putStrLnIO 
                ("withScriptData: Attempted to use entityID " ++ show entityID 
                    ++ "'s script data of type " ++ show dynScriptData 
                    ++ " with a function that accepts a different type.")

editScriptData :: (Typeable a, MonadIO m, MonadState World m) =>
                    EntityID -> (a -> m a) -> m ()
editScriptData entityID f = 
    traverseM_ (use (wldComponents . cmpScriptData . at entityID)) $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> do
                newScriptData <- f scriptData
                wldComponents . cmpScriptData . at entityID ?= toDyn newScriptData
            Nothing -> do
                putStrLnIO 
                    ("editScriptData: Attempted to use entityID " ++ show entityID 
                        ++ "'s script data of type " ++ show dynScriptData 
                        ++ " with a function that accepts a different type.")
