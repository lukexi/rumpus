{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.CodeEditor where
import PreludeExtra hiding (Key)
import Data.ECS

import Graphics.GL.Freetype
import Halive.SubHalive
import Halive.Recompiler
import TinyRick

import Rumpus.Systems.Controls
import Rumpus.Systems.Selection
import Rumpus.Systems.Script
import Rumpus.Systems.Collisions
import Rumpus.Systems.Shared
import qualified Data.Map as Map
import Control.Monad.Trans.Maybe
import Data.Vault.Strict (Key)

-- | Pairs a filename along with an expression 
-- to evaluate in that filename's environment once compiled
type CodeFile = (FilePath, String)

data CodeEditor = CodeEditor
    { _cedResultTChan   :: TChan CompilationResult
    , _cedCodeRenderer  :: TextRenderer
    , _cedErrorRenderer :: TextRenderer
    , _cedDependents    :: Map EntityID (CompiledValue -> ECSMonad ())
    }
makeLenses ''CodeEditor

data CodeEditorSystem = CodeEditorSystem 
    { _cesCodeEditors :: !(Map CodeFile CodeEditor) 
    , _cesFont        :: !Font
    , _cesGHCChan     :: !(TChan CompilationRequest)
    }
makeLenses ''CodeEditorSystem

defineSystemKey ''CodeEditorSystem

defineComponentKeyWithType "OnStartExpr"     [t|CodeFile|]
defineComponentKeyWithType "OnUpdateExpr"    [t|CodeFile|]
defineComponentKeyWithType "OnCollisionExpr" [t|CodeFile|]
defineComponentKeyWithType "OnCollisionStartExpr" [t|CodeFile|]

addCodeExpr :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) 
            => FilePath
            -> String
            -> Key (EntityMap CodeFile)
            -> Key (EntityMap a)
            -> m ()
addCodeExpr fileName exprName codeFileComponentKey codeComponentKey = do
    entityID <- ask
    let defaultFile = "resources" </> "default-code" </> "Default" ++ fileName <.> ".hs"
        entityFileName = show entityID ++ "-" ++ fileName ++ ".hs"
        codeFile = (entityFileName, exprName)
    contents <- liftIO $ readFile defaultFile
    liftIO $ writeFile entityFileName contents
    codeFileComponentKey ==> codeFile
    registerWithCodeEditor codeFile codeComponentKey

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
    registerCodeExprComponent "OnStartExpr"     cmpOnStartExpr     cmpOnStart
    registerCodeExprComponent "OnUpdateExpr"    cmpOnUpdateExpr    cmpOnUpdate
    registerCodeExprComponent "OnCollisionExpr" cmpOnCollisionExpr cmpOnCollision
    registerCodeExprComponent "OnCollisionStartExpr" cmpOnCollisionStartExpr cmpOnCollisionStart

registerCodeExprComponent :: MonadState ECS m 
                          => String
                          -> Key (EntityMap CodeFile) 
                          -> Key (EntityMap a) 
                          -> m ()
registerCodeExprComponent name codeFileComponentKey codeComponentKey = 
    registerComponent name codeFileComponentKey $ (savedComponentInterface codeFileComponentKey)
        { ciDeriveComponent  = Just (
            withComponent codeFileComponentKey $ \codeFileKey -> 
                registerWithCodeEditor codeFileKey codeComponentKey
            )
        , ciRemoveComponent = do
            withComponent codeFileComponentKey $ \codeFileKey -> 
                unregisterWithCodeEditor codeFileKey
            removeComponent codeFileComponentKey
        }

registerWithCodeEditor :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) 
                       => CodeFile
                       -> Key (EntityMap a)
                       -> m ()
registerWithCodeEditor codeFile codeComponentKey = modifySystemState sysCodeEditor $ do
    use (cesCodeEditors . at codeFile) >>= \case
        Just _ -> return ()
        Nothing -> do
            ghcChan <- use cesGHCChan
            font    <- use cesFont

            let (scriptPath, exprString) = codeFile
            resultTChan   <- recompilerForExpression ghcChan scriptPath exprString
            codeRenderer  <- textRendererFromFile font scriptPath
            errorRenderer <- createTextRenderer font (textBufferFromString "noFile" "")
            entityID <- ask
            let codeEditor = CodeEditor 
                    { _cedCodeRenderer = codeRenderer
                    , _cedErrorRenderer = errorRenderer
                    , _cedResultTChan = resultTChan 
                    , _cedDependents = Map.singleton entityID (\newValue -> do
                        setEntityComponent codeComponentKey (getCompiledValue newValue) entityID
                        )
                    }
            cesCodeEditors . at codeFile ?= codeEditor

unregisterWithCodeEditor :: (MonadReader EntityID m, MonadState ECS m) => CodeFile -> m ()
unregisterWithCodeEditor codeFile = modifySystemState sysCodeEditor $ do
    entityID <- ask
    cesCodeEditors . ix codeFile . cedDependents . at entityID .= Nothing

tickCodeEditorSystem :: (MonadIO m, MonadState ECS m) => m ()
tickCodeEditorSystem = withSystem_ sysControls $ \ControlsSystem{..} -> do
    -- Pass keyboard events to the selected entity's text editor, if it has one
    let events = _ctsEvents
        window = gpWindow _ctsVRPal


    mSelectedEntityID <- viewSystem sysSelection selSelectedEntityID
    forM mSelectedEntityID $ \selectedEntityID ->
        withEntityComponent selectedEntityID cmpOnUpdateExpr $ \codeFileKey ->
            modifySystemState sysCodeEditor $ 
                forM_ events $ \case
                    GLFWEvent e -> handleTextBufferEvent window e 
                        (cesCodeEditors . ix codeFileKey . cedCodeRenderer)
                    VREvent (VRKeyboardInputEvent chars) -> forM_ chars $ \char -> do
                        handleTextBufferEvent window (Character char)
                            (cesCodeEditors . ix codeFileKey . cedCodeRenderer)
                    _ -> return ()

-- | Update the world state with the result of the editor upon successful compilations
-- or update the error renderers for each code editor on failures
tickSyncCodeEditorSystem :: ECSMonad ()
tickSyncCodeEditorSystem = modifySystemState sysCodeEditor $ do
    font <- use cesFont

    traverseM_ (Map.toList <$> use cesCodeEditors) $ \(codeFileKey, editor) -> do
        tryReadTChanIO (editor ^. cedResultTChan) >>= \case
            Nothing -> return ()
            Just (Left errors) -> do
                let allErrors = unlines errors
                putStrLnIO allErrors
                errorRenderer <- createTextRenderer font (textBufferFromString "errorMessage" allErrors)
                cesCodeEditors . ix codeFileKey . cedErrorRenderer .= errorRenderer
            Just (Right compiledValue) -> do
                -- Clear the error renderer
                errorRenderer <- createTextRenderer font (textBufferFromString "errorMessage" "")
                cesCodeEditors . ix codeFileKey . cedErrorRenderer .= errorRenderer

                dependents <- use (cesCodeEditors . ix codeFileKey . cedDependents)
                lift $ forM_ dependents ($ compiledValue)


raycastCursor :: (MonadIO m, MonadState ECS m) => EntityID -> m Bool
raycastCursor handEntityID = fmap (fromMaybe False) $ runMaybeT $ do
    -- First, see if we can place a cursor into a text buffer.
    -- If not, then move onto the selection logic.
    selectedEntityID <- MaybeT $ viewSystem sysSelection selSelectedEntityID
    codeFileKey      <- MaybeT $ getEntityComponent selectedEntityID cmpOnUpdateExpr
    editor           <- MaybeT $ viewSystem sysCodeEditor (cesCodeEditors . at codeFileKey)
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
        cesCodeEditors . ix codeFileKey . cedCodeRenderer .= updatedRenderer
    
    return True
