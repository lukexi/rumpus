{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.CodeEditor where
import PreludeExtra
import Rumpus.Types
import Rumpus.Systems.Shared

import Graphics.GL.Freetype
import Halive.SubHalive
import Halive.Recompiler
import TinyRick

import qualified Data.Map as Map

createCodeEditorSystem :: IO (Font, TChan CompilationRequest)
createCodeEditorSystem = do
    ghcChan   <- startGHC []
    glyphProg <- createShaderProgram "resources/shaders/glyph.vert" "resources/shaders/glyph.frag"
    font      <- createFont "resources/fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    return (font, ghcChan)

createCodeEditor :: (MonadReader WorldStatic m, MonadIO m, MonadState World m) => CodeExpressionKey -> m CodeEditor
createCodeEditor codeExpressionKey = do

    maybeExisting <- use (wldCodeEditors . at codeExpressionKey)
    case maybeExisting of
        Just existing -> return existing
        Nothing -> do
            ghcChan <- view wlsGHCChan
            font    <- view wlsFont

            let (scriptPath, exprString) = codeExpressionKey
            resultTChan   <- recompilerForExpression ghcChan scriptPath exprString
            codeRenderer  <- textRendererFromFile font scriptPath
            errorRenderer <- createTextRenderer font (textBufferFromString "noFile" "")
            let codeEditor = CodeEditor 
                    { _cedCodeRenderer = codeRenderer
                    , _cedErrorRenderer = errorRenderer
                    , _cedResultTChan = resultTChan 
                    }
            wldCodeEditors . at codeExpressionKey ?= codeEditor
            return codeEditor

codeEditorSystem :: WorldMonad ()
codeEditorSystem = do
    -- Pass keyboard events to the selected entity's text editor, if it has one
    events <- use wldEvents
    window <- gpWindow <$> view wlsVRPal
    useTraverseM_ wldSelectedEntityID $ \selectedEntityID -> do
        useTraverseM_ (wldComponents . cmpOnUpdateExpr . at selectedEntityID) $ \codeExprKey ->
            forM_ events $ \case
                GLFWEvent e -> handleTextBufferEvent window e 
                    (wldCodeEditors . ix codeExprKey . cedCodeRenderer)
                VREvent (VRKeyboardInputEvent chars) -> forM_ chars $ \char -> do
                    handleTextBufferEvent window (Character char)
                        (wldCodeEditors . ix codeExprKey . cedCodeRenderer)
                _ -> return ()

-- | Update the world state with the result of the editor upon successful compilations
-- or update the error renderers for each code editor on failures
syncCodeEditorSystem :: WorldMonad ()
syncCodeEditorSystem = do
    font <- view wlsFont

    let copyCompiledResultToEntities codeExprKey value codeExprLens valueLens = 
            traverseM_ (Map.toList <$> use (wldComponents . codeExprLens)) $ \(entityID, entityCodeExprKey) -> do
                    when (entityCodeExprKey == codeExprKey) $ 
                        wldComponents . valueLens . at entityID ?= value

    traverseM_ (Map.toList <$> use wldCodeEditors) $ \(codeExprKey, editor) -> do
        tryReadTChanIO (editor ^. cedResultTChan) >>= \case
            Just (Left errors) -> do
                let allErrors = unlines errors
                putStrLnIO allErrors
                errorRenderer <- createTextRenderer font (textBufferFromString "errorMessage" allErrors)
                wldCodeEditors . ix codeExprKey . cedErrorRenderer .= errorRenderer
            Just (Right compiledValue) -> do
                -- Clear the error renderer
                errorRenderer <- createTextRenderer font (textBufferFromString "errorMessage" "")
                wldCodeEditors . ix codeExprKey . cedErrorRenderer .= errorRenderer

                let value = getCompiledValue compiledValue
                copyCompiledResultToEntities codeExprKey value cmpOnStartExpr     cmpOnStart
                copyCompiledResultToEntities codeExprKey value cmpOnUpdateExpr    cmpOnUpdate
                copyCompiledResultToEntities codeExprKey value cmpOnCollisionExpr cmpOnCollision
            Nothing -> return ()



    
