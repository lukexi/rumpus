{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.CodeEditor where
import PreludeExtra
import Rumpus.Types
import Rumpus.ECS

import Graphics.GL.Freetype
import Halive.SubHalive
import Halive.Recompiler
import TinyRick

import Rumpus.Control
import Rumpus.Systems.Selection
import Rumpus.Systems.Script
import Rumpus.Systems.Shared
import qualified Data.Map as Map

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

defineComponentKeyWithType "OnStartExpr" [t|CodeExpressionKey|]
defineComponentKeyWithType "OnUpdateExpr" [t|CodeExpressionKey|]
defineComponentKeyWithType "OnCollisionExpr" [t|CodeExpressionKey|]



createCodeEditorSystem :: IO (Font, TChan CompilationRequest)
createCodeEditorSystem = do
    ghcChan   <- startGHC []
    glyphProg <- createShaderProgram "resources/shaders/glyph.vert" "resources/shaders/glyph.frag"
    font      <- createFont "resources/fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    return (font, ghcChan)

createCodeEditor :: (MonadIO m, MonadState World m) => CodeExpressionKey -> m CodeEditor
createCodeEditor codeExpressionKey = modifySystemState codeEditorSystemKey $ do
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

tickCodeEditorSystem :: (MonadIO m, MonadState World m) => m ()
tickCodeEditorSystem = withSystem_ controlSystemKey $ \ControlSystem{..} -> do
    -- Pass keyboard events to the selected entity's text editor, if it has one
    let events = _ctsEvents
        window = gpWindow _ctsVRPal


    mSelectedEntityID <- viewSystem selectionSystemKey selSelectedEntityID
    forM mSelectedEntityID $ \selectedEntityID ->
        withComponent selectedEntityID onUpdateExprKey $ \codeExprKey ->
            modifySystemState codeEditorSystemKey $ 
                forM_ events $ \case
                    GLFWEvent e -> handleTextBufferEvent window e 
                        (cesCodeEditors . ix codeExprKey . cedCodeRenderer)
                    VREvent (VRKeyboardInputEvent chars) -> forM_ chars $ \char -> do
                        handleTextBufferEvent window (Character char)
                            (cesCodeEditors . ix codeExprKey . cedCodeRenderer)
                    _ -> return ()

-- | Update the world state with the result of the editor upon successful compilations
-- or update the error renderers for each code editor on failures
tickSyncCodeEditorSystem :: WorldMonad ()
tickSyncCodeEditorSystem = modifySystemState codeEditorSystemKey $ do
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
                lift $ copyCompiledResultToEntities codeExprKey value onStartExprKey     onStartKey
                lift $ copyCompiledResultToEntities codeExprKey value onUpdateExprKey    onUpdateKey
                lift $ copyCompiledResultToEntities codeExprKey value onCollisionExprKey onCollisionKey
            Nothing -> return ()

-- | Dummy leftover to illustrate adding expression editor for each script

addScriptComponent :: (MonadState World m, MonadIO m) => EntityID -> Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> m ()
addScriptComponent entityID mOnStart mOnUpdate mOnCollision = do

    forM_ (mOnStart) $ \scriptPath -> do

        let codeExprKey = (scriptPath, "start")
        _ <- createCodeEditor codeExprKey
        
        addComponent onStartExprKey codeExprKey entityID 
        

    forM_ (mOnUpdate) $ \scriptPath -> do
        let codeExprKey = (scriptPath, "update")
        _ <- createCodeEditor codeExprKey
        
        addComponent onUpdateExprKey codeExprKey entityID 

    forM_ (mOnCollision) $ \scriptPath -> do
        let codeExprKey = (scriptPath, "collision")
        _ <- createCodeEditor codeExprKey
        
        addComponent onCollisionExprKey codeExprKey entityID 


raycastCursor :: (MonadIO m, MonadState World m) => EntityID -> m Bool
raycastCursor handEntityID = modifySystemState codeEditorSystemKey $ do
    -- First, see if we can place a cursor into a text buffer.
    -- If not, then move onto the selection logic.
    mSelectedEntityID <- lift $ viewSystem selectionSystemKey selSelectedEntityID
    case mSelectedEntityID of
        Nothing -> return False
        Just selectedEntityID -> do
            maybeCodeExprKey <- lift $ getComponent selectedEntityID onUpdateExprKey
            case maybeCodeExprKey of
                Nothing -> return False
                Just codeExprKey -> do
                    maybeEditor <- use (cesCodeEditors . at codeExprKey)
                    case maybeEditor of
                        Nothing -> return False
                        Just editor -> do
                            handPose <- lift $ getEntityPose handEntityID
                            pose     <- lift $ getEntityPose selectedEntityID
                            -- We currently render code editors directly matched with the pose
                            -- of the entity; update this when we make code editors into their own entities
                            -- like the editorFrame children are
                            let model44 = transformationFromPose pose
                                codeRenderer = editor ^. cedCodeRenderer
                                handRay = poseToRay handPose (V3 0 0 (-1))
                            mUpdatedRenderer <- castRayToTextRenderer handRay codeRenderer model44
                            case mUpdatedRenderer of
                                Nothing -> return False
                                Just updatedRenderer -> do
                                    cesCodeEditors . ix codeExprKey . cedCodeRenderer .= updatedRenderer
                                    return True
