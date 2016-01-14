{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Rumpus.Systems.CodeEditor where
import Rumpus.Types
import Control.Monad.State
import Graphics.GL.Freetype
import Control.Concurrent.STM
import TinyRick.Recompiler2
import Graphics.GL.Pal
import TinyRick
import Control.Lens.Extra
import Graphics.VR.Pal
import Rumpus.Systems.Shared
import qualified Data.Map as Map

createCodeEditorSystem :: IO (Font, TChan CompilationRequest)
createCodeEditorSystem = do
    ghcChan   <- startGHC ["app"]
    glyphProg <- createShaderProgram "resources/shaders/glyph.vert" "resources/shaders/glyph.frag"
    font      <- createFont "resources/fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    return (font, ghcChan)



codeEditorSystem :: WorldMonad ()
codeEditorSystem = do
    -- Pass keyboard events to the selected entity's text editor, if it has one
    events <- use wldEvents
    window <- gpWindow <$> view wlsVRPal
    traverseM_ (use wldSelectedEntityID) $ \selectedEntityID ->
        forM_ events $ \case
            GLFWEvent e -> handleTextBufferEvent window e 
                (wldComponents . cmpOnUpdateEditor . ix selectedEntityID . cedCodeRenderer)
            _ -> return ()

syncCodeEditorSystem :: WorldMonad ()
syncCodeEditorSystem = do
    font <- view wlsFont

    -- Update the world state with the result of the editor upon successful compilations
    -- or update the error renderers for each code editor on failures
    let updateFromEditor :: Lens' Components (EntityMap CodeEditor) -> Lens' Components (EntityMap r) -> WorldMonad ()
        updateFromEditor editorLens valueLens =
            traverseM_ (Map.toList <$> use (wldComponents . editorLens)) $
                \(entityID, editor) -> 
                    fmap getCompilationResult <$> tryReadTChanIO (editor ^. cedResultTChan) >>= \case
                        Just (Left errors) -> do
                            errorRenderer <- createTextRenderer font (textBufferFromString "noFile" (unlines errors))
                            wldComponents . editorLens . ix entityID . cedErrorRenderer .= errorRenderer
                        Just (Right value) ->
                            wldComponents . valueLens . at entityID ?= value
                        Nothing -> return ()
    updateFromEditor cmpOnStartEditor     cmpOnStart
    updateFromEditor cmpOnUpdateEditor    cmpOnUpdate
    updateFromEditor cmpOnCollisionEditor cmpOnCollision
