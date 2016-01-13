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
            onStart entityID
            -- Only call OnStart once
            wldComponents . cmpOnStart . at entityID .= Nothing

    traverseM_ (Map.toList <$> use (wldComponents . cmpOnUpdate)) $ 
        \(entityID, onUpdate) -> 
            onUpdate entityID



addScriptComponent :: (MonadReader WorldStatic m, MonadState World m, MonadIO m) => EntityID -> Entity -> m ()
addScriptComponent entityID entity = do
    ghcChan <- view wlsGHCChan
    font    <- view wlsFont

    forM_ (entity ^. entOnUpdate) $ \scriptPath -> do
        resultTChan   <- recompilerForExpression ghcChan scriptPath "update"
        codeRenderer  <- textRendererFromFile font scriptPath
        errorRenderer <- createTextRenderer font (textBufferFromString "noFile" [])
        let editor = CodeEditor 
                { _cedCodeRenderer = codeRenderer
                , _cedErrorRenderer = errorRenderer
                , _cedResultTChan = resultTChan }
        
        
        wldComponents . cmpOnUpdateEditor . at entityID ?= editor

        -- FIXME: Just for testing til we get a proper selection system
        wldSelectedEntityID ?= entityID

