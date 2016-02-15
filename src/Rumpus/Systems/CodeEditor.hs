{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.CodeEditor where
import PreludeExtra
import Data.ECS

import Graphics.GL.Freetype
import Halive.SubHalive
import Halive.Recompiler
import TinyRick

import Rumpus.Systems.Controls
import Rumpus.Systems.Selection
import Rumpus.Systems.Script
import Rumpus.Systems.Physics
import Rumpus.Systems.Shared
import qualified Data.Map as Map
import Control.Monad.Trans.Maybe

-- | Pairs a filename along with an expression 
-- to evaluate in that filename's environment once compiled
type CodeExpressionKey = (FilePath, String)

data CodeEditor = CodeEditor
    { _cedResultTChan   :: TChan CompilationResult
    , _cedCodeRenderer  :: TextRenderer
    , _cedErrorRenderer :: TextRenderer
    }
makeLenses ''CodeEditor

data CodeEditorSystem = CodeEditorSystem 
    { _cesCodeEditors :: !(Map CodeExpressionKey CodeEditor) 
    , _cesFont        :: !Font
    , _cesGHCChan     :: !(TChan CompilationRequest)
    }
makeLenses ''CodeEditorSystem

defineSystemKey ''CodeEditorSystem

defineComponentKeyWithType "OnStartExpr"     [t|CodeExpressionKey|]
defineComponentKeyWithType "OnUpdateExpr"    [t|CodeExpressionKey|]
defineComponentKeyWithType "OnCollisionExpr" [t|CodeExpressionKey|]



initCodeEditorSystem :: (MonadIO m, MonadState ECS m) => m ()
initCodeEditorSystem = do
    ghcChan   <- startGHC []
    glyphProg <- createShaderProgram "resources/shaders/glyph.vert" "resources/shaders/glyph.frag"
    font      <- createFont "resources/fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    registerSystem sysCodeEditor $ CodeEditorSystem
        { _cesCodeEditors = mempty
        , _cesFont = font
        , _cesGHCChan = ghcChan
        }

    -- Will require (scriptPath, "start") (or "update" or "collision") to be added somewhere!
    let registerCodeExprComponent name componentKey = 
            registerComponent name componentKey $ (savedComponentInterface componentKey)
                { ciDeriveComponent  = Just (\entityID -> do
                    withComponent entityID componentKey $ \codeExprKey -> do
                        _ <- createCodeEditor codeExprKey
                        return ()
                    )
                }

    registerCodeExprComponent "OnStartExpr" cmpOnStartExpr
    registerCodeExprComponent "OnUpdateExpr" cmpOnUpdateExpr
    registerCodeExprComponent "OnCollisionExpr" cmpOnCollisionExpr

createCodeEditor :: (MonadIO m, MonadState ECS m) => CodeExpressionKey -> m CodeEditor
createCodeEditor codeExpressionKey = modifySystemState sysCodeEditor $ do
    mEditor <- use (cesCodeEditors . at codeExpressionKey)
    case mEditor of
        Just existing -> return existing
        Nothing -> do
            ghcChan <- use cesGHCChan
            font    <- use cesFont

            let (scriptPath, exprString) = codeExpressionKey
            resultTChan   <- recompilerForExpression ghcChan scriptPath exprString
            codeRenderer  <- textRendererFromFile font scriptPath
            errorRenderer <- createTextRenderer font (textBufferFromString "noFile" "")
            let codeEditor = CodeEditor 
                    { _cedCodeRenderer = codeRenderer
                    , _cedErrorRenderer = errorRenderer
                    , _cedResultTChan = resultTChan 
                    }
            cesCodeEditors . at codeExpressionKey ?= codeEditor
            return codeEditor

tickCodeEditorSystem :: (MonadIO m, MonadState ECS m) => m ()
tickCodeEditorSystem = withSystem_ sysControls $ \ControlsSystem{..} -> do
    -- Pass keyboard events to the selected entity's text editor, if it has one
    let events = _ctsEvents
        window = gpWindow _ctsVRPal


    mSelectedEntityID <- viewSystem sysSelection selSelectedEntityID
    forM mSelectedEntityID $ \selectedEntityID ->
        withComponent selectedEntityID cmpOnUpdateExpr $ \codeExprKey ->
            modifySystemState sysCodeEditor $ 
                forM_ events $ \case
                    GLFWEvent e -> handleTextBufferEvent window e 
                        (cesCodeEditors . ix codeExprKey . cedCodeRenderer)
                    VREvent (VRKeyboardInputEvent chars) -> forM_ chars $ \char -> do
                        handleTextBufferEvent window (Character char)
                            (cesCodeEditors . ix codeExprKey . cedCodeRenderer)
                    _ -> return ()

-- | Update the world state with the result of the editor upon successful compilations
-- or update the error renderers for each code editor on failures
tickSyncCodeEditorSystem :: ECSMonad ()
tickSyncCodeEditorSystem = modifySystemState sysCodeEditor $ do
    font <- use cesFont

    let copyCompiledResultToEntities codeExprKey value comCodeExpr comCode = 
            forEntitiesWithComponent comCodeExpr $ 
                \(entityID, entityCodeExprKey) -> do
                    when (entityCodeExprKey == codeExprKey) $ 
                        setComponent comCode value entityID

    traverseM_ (Map.toList <$> use cesCodeEditors) $ \(codeExprKey, editor) -> do
        tryReadTChanIO (editor ^. cedResultTChan) >>= \case
            Just (Left errors) -> do
                let allErrors = unlines errors
                putStrLnIO allErrors
                errorRenderer <- createTextRenderer font (textBufferFromString "errorMessage" allErrors)
                cesCodeEditors . ix codeExprKey . cedErrorRenderer .= errorRenderer
            Just (Right compiledValue) -> do
                -- Clear the error renderer
                errorRenderer <- createTextRenderer font (textBufferFromString "errorMessage" "")
                cesCodeEditors . ix codeExprKey . cedErrorRenderer .= errorRenderer

                let value = getCompiledValue compiledValue
                lift $ copyCompiledResultToEntities codeExprKey value cmpOnStartExpr     cmpOnStart
                lift $ copyCompiledResultToEntities codeExprKey value cmpOnUpdateExpr    cmpOnUpdate
                lift $ copyCompiledResultToEntities codeExprKey value cmpOnCollisionExpr cmpOnCollision
            Nothing -> return ()



raycastCursor :: (MonadIO m, MonadState ECS m) => EntityID -> m Bool
raycastCursor handEntityID = fmap (fromMaybe False) $ runMaybeT $ do
    -- First, see if we can place a cursor into a text buffer.
    -- If not, then move onto the selection logic.
    selectedEntityID <- MaybeT $ viewSystem sysSelection selSelectedEntityID
    codeExprKey      <- MaybeT $ getComponent selectedEntityID cmpOnUpdateExpr
    editor           <- MaybeT $ viewSystem sysCodeEditor (cesCodeEditors . at codeExprKey)
    handPose         <- getEntityPose handEntityID
    pose             <- getEntityPose selectedEntityID
    
    -- We currently render code editors directly matched with the pose
    -- of the entity; update this when we make code editors into their own entities
    -- like the editorFrame children are
    let model44 = transformationFromPose pose
        codeRenderer = editor ^. cedCodeRenderer
        handRay = poseToRay handPose (V3 0 0 (-1))
    updatedRenderer  <- MaybeT $ castRayToTextRenderer handRay codeRenderer model44
    
    modifySystemState sysCodeEditor $ 
        cesCodeEditors . ix codeExprKey . cedCodeRenderer .= updatedRenderer
    
    return True
