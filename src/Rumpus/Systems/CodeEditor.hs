{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Rumpus.Systems.CodeEditor where
import PreludeExtra
import Rumpus.Types
import Rumpus.Systems.Shared

import Graphics.GL.Freetype
import Control.Concurrent.STM
import Halive.SubHalive
import Graphics.GL.Pal
import TinyRick

import qualified Data.Map as Map

createCodeEditorSystem :: IO (Font, TChan CompilationRequest)
createCodeEditorSystem = do
    ghcChan   <- startGHC []
    glyphProg <- createShaderProgram "resources/shaders/glyph.vert" "resources/shaders/glyph.frag"
    font      <- createFont "resources/fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    return (font, ghcChan)

lookupCodeEditor :: (MonadReader WorldStatic m, MonadIO m) => CodeExpressionKey -> m CodeEditor
lookupCodeEditor codeExpressionKey = do

    maybeExisting <- use (wldCodeEditors . at codeExpressionKey)
    case maybeExisting of
        Just existing -> return existing
        Nothing -> do
            ghcChan <- view wlsGHCChan
            font    <- view wlsFont

            resultTChan   <- recompilerForExpression ghcChan scriptPath exprString
            codeRenderer  <- textRendererFromFile font scriptPath
            errorRenderer <- createTextRenderer font (textBufferFromString "noFile" "")
            return CodeEditor 
                    { _cedCodeRenderer = codeRenderer
                    , _cedErrorRenderer = errorRenderer
                    , _cedResultTChan = resultTChan 
                    }

codeEditorSystem :: WorldMonad ()
codeEditorSystem = do
    -- Pass keyboard events to the selected entity's text editor, if it has one
    events <- use wldEvents
    window <- gpWindow <$> view wlsVRPal
    traverseM_ (use wldSelectedEntityID) $ \selectedEntityID ->
        forM_ events $ \case
            GLFWEvent e -> handleTextBufferEvent window e 
                (wldComponents . cmpOnUpdateEditor . ix selectedEntityID . cedCodeRenderer)
            VREvent (VRKeyboardInputEvent chars) -> forM_ chars $ \char -> do
                handleTextBufferEvent window (Character char)
                    (wldComponents . cmpOnUpdateEditor . ix selectedEntityID . cedCodeRenderer)
            _ -> return ()

-- | Update the world state with the result of the editor upon successful compilations
-- or update the error renderers for each code editor on failures
syncCodeEditorSystem :: WorldMonad ()
syncCodeEditorSystem = do
    font <- view wlsFont
    let updateFromEditor :: Lens' Components (EntityMap CodeEditor) -> Lens' Components (EntityMap r) -> WorldMonad ()
        updateFromEditor editorLens valueLens =
            traverseM_ (Map.toList <$> use (wldComponents . editorLens)) $
                \(entityID, editor) -> 
                    fmap getCompilationResult <$> tryReadTChanIO (editor ^. cedResultTChan) >>= \case
                        Just (Left errors) -> do
                            let allErrors = unlines errors
                            putStrLnIO allErrors
                            errorRenderer <- createTextRenderer font (textBufferFromString "noFile" allErrors)
                            wldComponents . editorLens . ix entityID . cedErrorRenderer .= errorRenderer
                        Just (Right value) ->
                            wldComponents . valueLens . at entityID ?= value
                        Nothing -> return ()
    updateFromEditor cmpOnStartEditor     cmpOnStart
    updateFromEditor cmpOnUpdateEditor    cmpOnUpdate
    updateFromEditor cmpOnCollisionEditor cmpOnCollision
