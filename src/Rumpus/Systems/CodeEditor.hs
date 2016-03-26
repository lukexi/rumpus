{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.CodeEditor where
import PreludeExtra hiding (Key)

import Graphics.GL.Freetype
import Graphics.GL.TextBuffer
import Halive.SubHalive
import Halive.Recompiler
import Halive.FileListener

import qualified Data.HashMap.Strict as Map
import Control.Monad.Trans.Maybe
import Data.ECS.Vault

import Rumpus.Systems.Controls
import Rumpus.Systems.Selection
import Rumpus.Systems.Script
import Rumpus.Systems.Collisions
import Rumpus.Systems.Shared
import Rumpus.Systems.PlayPause

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

defineComponentKeyWithType "OnStartExpr"          [t|CodeFile|]
defineComponentKeyWithType "OnUpdateExpr"         [t|CodeFile|]
defineComponentKeyWithType "OnCollisionExpr"      [t|CodeFile|]
defineComponentKeyWithType "OnCollisionStartExpr" [t|CodeFile|]

defineComponentKeyWithType "PlayWhenReady" [t|Bool|]

getPlayWhenReady :: (MonadState ECS m, MonadReader EntityID m) => m Bool
getPlayWhenReady = fromMaybe False <$> getComponent cmpPlayWhenReady

getEntityPlayWhenReady :: (MonadState ECS m) => EntityID -> m Bool
getEntityPlayWhenReady entityID = fromMaybe False <$> getEntityComponent entityID cmpPlayWhenReady



addCodeExpr :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) 
            => FilePath
            -> String
            -> Key (EntityMap CodeFile)
            -> Key (EntityMap a)
            -> m ()
addCodeExpr fileName exprName codeFileComponentKey codeComponentKey = do
    sceneFolder <- viewSystem sysSelection (selScene . scnFolder)
    entityID <- ask
    let defaultFile = "resources" </> "default-code" </> "Default" ++ fileName <.> "hs"
        entityFileName = sceneFolder </> (show entityID ++ "-" ++ fileName) <.> "hs"
        codeFile = (entityFileName, exprName)
    contents <- liftIO $ readFile defaultFile
    liftIO $ writeFile entityFileName contents
    codeFileComponentKey ==> codeFile
    registerWithCodeEditor codeFile codeComponentKey

initCodeEditorSystem :: (MonadIO m, MonadState ECS m) => m ()
initCodeEditorSystem = do

    -- Check for the existence of a "packages" directory to see if we're
    -- inside of a standalone distrubtion of rumpus versus building from the repo.
    isStandaloneRelease <- liftIO $ doesDirectoryExist "packages"

    let ghcSessionConfig = if isStandaloneRelease 
            then defaultGHCSessionConfig 
                { gscPackageDBs = [ "packages"</>"local"</>"pkgdb"
                                  , "packages"</>"snapshot"</>"pkgdb"
                                  ]
                , gscLibDir = "packages"</>"ghc"</>"lib"
                , gscFixDebounce = DebounceFix
                }
            else defaultGHCSessionConfig 
                { gscFixDebounce = DebounceFix 
                }

    ghcChan   <- startGHC ghcSessionConfig
    glyphProg <- createShaderProgram "resources/shaders/glyph.vert" "resources/shaders/glyph.frag"
    font      <- createFont "resources/fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    registerSystem sysCodeEditor $ CodeEditorSystem
        { _cesCodeEditors = mempty
        , _cesFont = font
        , _cesGHCChan = ghcChan
        }

    -- Will require (scriptPath, "start") (or "update" or "collision") to be added somewhere!
    registerCodeExprComponent "OnStartExpr"          cmpOnStartExpr          cmpOnStart
    registerCodeExprComponent "OnUpdateExpr"         cmpOnUpdateExpr         cmpOnUpdate
    registerCodeExprComponent "OnCollisionExpr"      cmpOnCollisionExpr      cmpOnCollision
    registerCodeExprComponent "OnCollisionStartExpr" cmpOnCollisionStartExpr cmpOnCollisionStart

    registerComponent "PlayWhenReady" cmpPlayWhenReady (savedComponentInterface cmpPlayWhenReady)


registerCodeExprComponent :: MonadState ECS m 
                          => String
                          -> Key (EntityMap CodeFile) 
                          -> Key (EntityMap a) 
                          -> m ()
registerCodeExprComponent name codeFileComponentKey codeComponentKey = 
    registerComponent name codeFileComponentKey $ (savedComponentInterface codeFileComponentKey)
        { ciDeriveComponent  = Just (
            withComponent_ codeFileComponentKey $ \codeFileKey -> 
                registerWithCodeEditor codeFileKey codeComponentKey
            )
        , ciRemoveComponent = do
            withComponent_ codeFileComponentKey $ \codeFileKey -> 
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
            codeEditor <- createCodeEditor codeFile
            cesCodeEditors . at codeFile ?= codeEditor
    addCodeEditorDependency codeFile codeComponentKey

addCodeEditorDependency :: (MonadState CodeEditorSystem m, MonadReader EntityID m) 
                        => (FilePath, String) -> Key (EntityMap a) -> m ()
addCodeEditorDependency codeFile codeComponentKey = do
    entityID <- ask
    let updateCode newValue = do
            putStrLnIO $ "Setting code  " ++ show codeFile ++ " on entity: " ++ show entityID
            setEntityComponent codeComponentKey (getCompiledValue newValue) entityID

            -- FIXME: scratch pass at a "PlayWhenReady" system
            -- to begin play when a file has PlayWhenReady set
            -- and its start function finished compiling
            when (snd codeFile == "start") $ do
                playWhenReady <- getEntityPlayWhenReady entityID
                when playWhenReady $ setWorldPlaying True
    cesCodeEditors . at codeFile . traverse . cedDependents . at entityID ?= updateCode

createCodeEditor :: (MonadIO m, MonadState CodeEditorSystem m) 
                 => (FilePath, String) -> m CodeEditor
createCodeEditor codeFile = do
    ghcChan <- use cesGHCChan
    font    <- use cesFont

    let (scriptPath, exprString) = codeFile
    resultTChan   <- recompilerForExpression ghcChan scriptPath exprString
    codeRenderer  <- textRendererFromFile font scriptPath WatchFile
    errorRenderer <- createTextRenderer font (textBufferFromString "")
    
    return CodeEditor 
            { _cedCodeRenderer = codeRenderer
            , _cedErrorRenderer = errorRenderer
            , _cedResultTChan = resultTChan 
            , _cedDependents = mempty
            }  

unregisterWithCodeEditor :: (MonadReader EntityID m, MonadState ECS m) => CodeFile -> m ()
unregisterWithCodeEditor codeFile = modifySystemState sysCodeEditor $ do
    entityID <- ask
    cesCodeEditors . ix codeFile . cedDependents . at entityID .= Nothing

-- | Passes keyboard events to the active code editor
tickCodeEditorInputSystem :: (MonadIO m, MonadState ECS m) => m ()
tickCodeEditorInputSystem = withSystem_ sysControls $ \ControlsSystem{..} -> do
    let events = _ctsEvents
        window = gpWindow _ctsVRPal


    mSelectedEntityID <- viewSystem sysSelection selSelectedEntityID
    forM mSelectedEntityID $ \selectedEntityID ->
        withEntityComponent selectedEntityID cmpOnStartExpr $ \codeFileKey ->
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
tickCodeEditorResultsSystem :: ECSMonad ()
tickCodeEditorResultsSystem = modifySystemState sysCodeEditor $ do
    font <- use cesFont

    traverseM_ (Map.toList <$> use cesCodeEditors) $ \(codeFileKey, editor) -> do
        -- Ensure the buffers have the latest code text from disk
        refreshTextRendererFromFile (cesCodeEditors . ix codeFileKey . cedCodeRenderer)

        -- Update entities with new code from the compiler
        tryReadTChanIO (editor ^. cedResultTChan) >>= \case
            Nothing -> return ()
            Just (Left errors) -> do
                let allErrors = unlines errors
                putStrLnIO allErrors
                setTextRendererText (cesCodeEditors . ix codeFileKey . cedErrorRenderer) allErrors
            Just (Right compiledValue) -> do
                -- Clear the error renderer
                setTextRendererText (cesCodeEditors . ix codeFileKey . cedErrorRenderer) ""
                
                -- Pass the compiled value to each registered "dependent" of the code editor
                dependents <- use (cesCodeEditors . ix codeFileKey . cedDependents)
                lift $ forM_ dependents ($ compiledValue)


raycastCursor :: (MonadIO m, MonadState ECS m) => EntityID -> m Bool
raycastCursor handEntityID = fmap (fromMaybe False) $ runMaybeT $ do
    -- First, see if we can place a cursor into a text buffer.
    -- If not, then move onto the selection logic.
    selectedEntityID <- MaybeT $ viewSystem sysSelection selSelectedEntityID
    codeFileKey      <- MaybeT $ getEntityComponent selectedEntityID cmpOnStartExpr
    editor           <- MaybeT $ viewSystem sysCodeEditor (cesCodeEditors . at codeFileKey)
    handPose         <- getEntityPose handEntityID
    pose             <- getEntityPose selectedEntityID
    
    -- We currently render code editors directly matched with the pose
    -- of the entity; update this when we make code editors into their own entities
    -- like the editorFrame children are
    let model44 = pose
        codeRenderer = editor ^. cedCodeRenderer
        handRay = poseToRay (poseFromMatrix handPose) (V3 0 0 (-1))
    updatedRenderer  <- setCursorTextRendererWithRay handRay codeRenderer model44

    modifySystemState sysCodeEditor $ 
        cesCodeEditors . ix codeFileKey . cedCodeRenderer .= updatedRenderer
    
    return True
