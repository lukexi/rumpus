{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Script where
import Rumpus.Types
import qualified Data.Map as Map
import Control.Lens.Extra
import Control.Monad.State
import Control.Monad.Reader
import TinyRick.Recompiler2
import TinyRick
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
        editor <- createCodeEditor scriptPath "start"
        
        wldComponents . cmpOnStartEditor . at entityID ?= editor

    forM_ (entity ^. entOnUpdate) $ \scriptPath -> do
        editor <- createCodeEditor scriptPath "update"
        
        wldComponents . cmpOnUpdateEditor . at entityID ?= editor

        -- FIXME: Just for testing til we get a proper selection system
        wldSelectedEntityID ?= entityID

createCodeEditor :: (MonadReader WorldStatic m, MonadIO m) => FilePath -> String -> m CodeEditor
createCodeEditor scriptPath exprString = do
    ghcChan <- view wlsGHCChan
    font    <- view wlsFont

    resultTChan   <- recompilerForExpression ghcChan scriptPath exprString
    codeRenderer  <- textRendererFromFile font scriptPath
    errorRenderer <- createTextRenderer font (textBufferFromString "noFile" [])
    return CodeEditor 
            { _cedCodeRenderer = codeRenderer
            , _cedErrorRenderer = errorRenderer
            , _cedResultTChan = resultTChan }
