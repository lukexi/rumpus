{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.CodeEditor where
import PreludeExtra hiding (Key)

import Graphics.GL.TextBuffer
import Halive.SubHalive
import Halive.Recompiler
import Halive.FileListener

import qualified Data.HashMap.Strict as Map
import Control.Monad.Trans.Maybe
import Data.ECS.Vault

import Rumpus.Systems.Controls
import Rumpus.Systems.Selection
import Rumpus.Systems.Collisions
import Rumpus.Systems.Shared
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Text
import Rumpus.Types

import Data.Time.Clock.POSIX

-- | Pairs a filename along with an expression 
-- to evaluate in that filename's environment once compiled
type CodeInFile = (FilePath, String)

data CodeEditor = CodeEditor
    { _cedRecompiler    :: !Recompiler
    , _cedCompiledValue :: !(Maybe CompiledValue)
    , _cedCodeRenderer  :: !TextRenderer
    , _cedErrorRenderer :: !TextRenderer
    , _cedDependents    :: !(Map EntityID (CompiledValue -> ECSMonad ()))
    }
makeLenses ''CodeEditor

data CodeEditorSystem = CodeEditorSystem 
    { _cesCodeEditors :: !(Map CodeInFile CodeEditor) 
    , _cesGHCChan     :: !(TChan CompilationRequest)
    }
makeLenses ''CodeEditorSystem

defineSystemKey ''CodeEditorSystem

defineComponentKeyWithType "OnStartExpr"          [t|CodeInFile|]
defineComponentKeyWithType "OnUpdateExpr"         [t|CodeInFile|]
defineComponentKeyWithType "OnCollisionExpr"      [t|CodeInFile|]
defineComponentKeyWithType "OnCollisionStartExpr" [t|CodeInFile|]

defineComponentKeyWithType "PlayWhenReady" [t|Bool|]

getPlayWhenReady :: (MonadState ECS m, MonadReader EntityID m) => m Bool
getPlayWhenReady = fromMaybe False <$> getComponent myPlayWhenReady

getEntityPlayWhenReady :: (MonadState ECS m) => EntityID -> m Bool
getEntityPlayWhenReady entityID = fromMaybe False <$> getEntityComponent entityID myPlayWhenReady

addCodeExpr :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) 
            => FilePath
            -> String
            -> Key (EntityMap CodeInFile)
            -> Key (EntityMap a)
            -> m ()
addCodeExpr fileName exprName codeFileComponentKey codeComponentKey = do
    sceneFolder <- getSceneFolder
    entityID <- ask
    let defaultFileName = "resources" </> "default-code" </> "Default" ++ fileName <.> "hs"
        entityFileName = sceneFolder </> (show entityID ++ "-" ++ fileName) <.> "hs"
        codeFile = (entityFileName, exprName)
    liftIO $ copyFile defaultFileName entityFileName
    codeFileComponentKey ==> codeFile
    registerWithCodeEditor codeFile codeComponentKey

forkCode :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> m ()
forkCode fromEntityID toEntityID = do
    let codeFileComponentKey = myOnStartExpr
    mCodeExpr <- getEntityComponent fromEntityID codeFileComponentKey

    forM_ mCodeExpr $ \(fullPath, expr) -> do
        let (path, fileName) = splitFileName fullPath
            (name, ext)      = splitExtension fileName
        -- Slightly tricky to get right without overwriting files; need to enumerate directory and find unused name
        -- so using getPosixTime for now. 
        --fileNum          = succ . fromMaybe 1 . readMaybe . reverse . takeWhile isDigit . reverse $ fileName
        now <- liftIO $ getPOSIXTime
        let newName = name ++ show now
            newFullPath = path </> newName <.> ext
        liftIO $ copyFile fullPath newFullPath

        let newCodeInFile = (newFullPath, expr)
        runEntity toEntityID $ do
            withComponent_ codeFileComponentKey unregisterWithCodeEditor
            codeFileComponentKey ==> newCodeInFile
            registerWithCodeEditor newCodeInFile codeFileComponentKey




initCodeEditorSystem :: (MonadIO m, MonadState ECS m) => m ()
initCodeEditorSystem = do

    -- When in release mode, use the embedded "packages" directory,
    -- otherwise use MSYS2's copy in /usr/local/ghc
    let ghcSessionConfig = if isInReleaseMode 
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
    

    registerSystem sysCodeEditor $ CodeEditorSystem
        { _cesCodeEditors = mempty
        , _cesGHCChan     = ghcChan
        }

    -- Will require (scriptPath, "start") (or "update" or "collision") to be added somewhere!
    registerCodeExprComponent "OnStartExpr"          myOnStartExpr          myOnStart
    registerCodeExprComponent "OnUpdateExpr"         myOnUpdateExpr         myOnUpdate
    registerCodeExprComponent "OnCollisionExpr"      myOnCollisionExpr      myOnCollision
    registerCodeExprComponent "OnCollisionStartExpr" myOnCollisionStartExpr myOnCollisionStart

    registerComponent "PlayWhenReady" myPlayWhenReady (savedComponentInterface myPlayWhenReady)


registerCodeExprComponent :: MonadState ECS m 
                          => String
                          -> Key (EntityMap CodeInFile) 
                          -> Key (EntityMap a) 
                          -> m ()
registerCodeExprComponent name codeInFileKey realCodeKey = 
    registerComponent name codeInFileKey $ (savedComponentInterface codeInFileKey)
        { ciDeriveComponent  = Just (
            withComponent_ codeInFileKey $ \codeInFile -> 
                registerWithCodeEditor codeInFile realCodeKey
            )
        , ciRemoveComponent = do
            withComponent_ codeInFileKey $ \codeInFile -> 
                unregisterWithCodeEditor codeInFile
            removeComponent codeInFileKey
        }

registerWithCodeEditor :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) 
                       => CodeInFile
                       -> Key (EntityMap a)
                       -> m ()
registerWithCodeEditor codeInFile realCodeKey = modifySystemState sysCodeEditor $ do

    use (cesCodeEditors . at codeInFile) >>= \case
        Just existingEditor -> do
            forM_ (existingEditor ^. cedCompiledValue) $ \compiledValue -> 
                lift $ setComponent realCodeKey (getCompiledValue compiledValue)
        Nothing -> do
            codeEditor <- createCodeEditor codeInFile
            cesCodeEditors . at codeInFile ?= codeEditor
    addCodeEditorDependency codeInFile realCodeKey

addCodeEditorDependency :: (MonadState CodeEditorSystem m, MonadReader EntityID m) 
                        => CodeInFile -> Key (EntityMap a) -> m ()
addCodeEditorDependency codeInFile realCodeKey = do
    entityID <- ask
    let updateCodeAction newValue = do
            putStrLnIO $ "Setting code  " ++ show codeInFile ++ " on entity: " ++ show entityID
            setEntityComponent realCodeKey (getCompiledValue newValue) entityID

            -- FIXME: scratch pass at a "PlayWhenReady" system
            -- to begin play when a file has PlayWhenReady set
            -- and its start function finished compiling
            playWhenReady <- getEntityPlayWhenReady entityID
            when (playWhenReady && snd codeInFile == "start") $
                setWorldPlaying True
    cesCodeEditors . at codeInFile . traverse . cedDependents . at entityID ?= updateCodeAction

fullPathForCodeInFile :: MonadState ECS m => (FilePath, String) -> m (FilePath, String)
fullPathForCodeInFile (fileName, exprString) = do
    sceneFolder <- getSceneFolder
    return (sceneFolder </> fileName, exprString)

createCodeEditor :: (MonadTrans t, MonadIO (t m), MonadState ECS m, MonadState CodeEditorSystem (t m)) 
                 => CodeInFile -> t m CodeEditor
createCodeEditor codeInFile = do
    ghcChan <- use cesGHCChan
    font <- lift getFont
    (scriptFullPath, exprString) <- lift $ fullPathForCodeInFile codeInFile
     
    recompiler   <- recompilerForExpression ghcChan scriptFullPath exprString

    -- FIXME this should be async...
    -- (note: could implement that using WatchFile/refreshText by writing a phony event)
    codeRenderer  <- textRendererFromFile font scriptFullPath WatchFile
    errorRenderer <- createTextRenderer font (textBufferFromString "")
    
    return CodeEditor 
            { _cedCodeRenderer  = codeRenderer
            , _cedErrorRenderer = errorRenderer
            , _cedRecompiler    = recompiler
            , _cedCompiledValue = Nothing 
            , _cedDependents    = mempty
            }

unregisterWithCodeEditor :: (MonadReader EntityID m, MonadState ECS m) => CodeInFile -> m ()
unregisterWithCodeEditor codeInFile = modifySystemState sysCodeEditor $ do
    entityID <- ask
    unregisterEntityWithCodeEditor entityID codeInFile

unregisterEntityWithCodeEditor :: (MonadState CodeEditorSystem m) => EntityID -> CodeInFile -> m ()
unregisterEntityWithCodeEditor entityID codeInFile = cesCodeEditors . ix codeInFile . cedDependents . at entityID .= Nothing

-- | Passes keyboard events to the active code editor
tickCodeEditorInputSystem :: (MonadIO m, MonadState ECS m) => m ()
tickCodeEditorInputSystem = withSystem_ sysControls $ \ControlsSystem{..} -> do
    let events = _ctsEvents
        window = gpWindow _ctsVRPal


    mSelectedEntityID <- viewSystem sysSelection selSelectedEntityID
    forM mSelectedEntityID $ \selectedEntityID ->
        withEntityComponent selectedEntityID myOnStartExpr $ \codeInFile ->
            modifySystemState sysCodeEditor $ do
                
                didSave <- fmap or . forM events $ \case
                    GLFWEvent e -> do
                        -- Make sure our events don't trigger reloading/recompilation
                        -- (fixme: should be able to ask text buffer whether event will trigger save and only pause if so)
                        pauseFileWatchers codeInFile
                        handleTextBufferEvent window e 
                            (cesCodeEditors . ix codeInFile . cedCodeRenderer)
                    _ -> return False
                when didSave $ do
                    recompileCodeInFile codeInFile

pauseFileWatchers :: (MonadIO m, MonadState CodeEditorSystem m) => CodeInFile -> m ()
pauseFileWatchers codeInFile = useTraverseM_ (cesCodeEditors . at codeInFile) $ \codeEditor -> do
    setIgnoreTimeNow (codeEditor ^. cedRecompiler . to recFileEventListener)
    forM_ (codeEditor ^. cedCodeRenderer . txrFileEventListener) setIgnoreTimeNow 

recompileCodeInFile :: (MonadTrans t, MonadIO (t m), MonadState ECS m, MonadState CodeEditorSystem (t m)) 
                    => CodeInFile -> t m ()
recompileCodeInFile codeInFile = useTraverseM_ (cesCodeEditors . at codeInFile) $ \codeEditor -> do
    ghcChan <- use cesGHCChan

    (fullPath, exprString) <- lift $ fullPathForCodeInFile codeInFile

    let resultsChan = codeEditor ^. cedRecompiler . to recResultTChan
        textBuffer  = codeEditor ^. cedCodeRenderer . txrTextBuffer
        compilationRequest = CompilationRequest
            { crResultTChan      = resultsChan
            -- NOTE: we want to make sure this isn't evaluated on this thread. Let the SubHalive thread do it:
            , crFileContents     = Just (stringFromTextBuffer textBuffer)
            , crFilePath         = fullPath
            , crExpressionString = exprString
            }

    writeTChanIO ghcChan compilationRequest


-- | Allows Script system to pass runtime exceptions to the error pane, 
-- assuming the given entityID has an onStartExpr.
setErrorText :: (MonadIO m, MonadState ECS m) => EntityID -> String -> m ()
setErrorText entityID errors = do
    traverseM_ (getEntityComponent entityID myOnStartExpr) $ \codeInFile ->
        modifySystemState sysCodeEditor $ 
            setTextRendererText (cesCodeEditors . ix codeInFile . cedErrorRenderer) errors

-- | Update the world state with the result of the editor upon successful compilations
-- or update the error renderers for each code editor on failures
tickCodeEditorResultsSystem :: ECSMonad ()
tickCodeEditorResultsSystem = modifySystemState sysCodeEditor $ 
    traverseM_ (Map.toList <$> use cesCodeEditors) $ \(codeInFile, editor) -> do
        -- Ensure the buffers have the latest code text from disk
        refreshTextRendererFromFile (cesCodeEditors . ix codeInFile . cedCodeRenderer)

        -- Update entities with new code from the compiler
        tryReadTChanIO (editor ^. cedRecompiler . to recResultTChan) >>= \case
            Nothing -> return ()
            Just (Left errors) -> do
                let allErrors = unlines errors
                putStrLnIO allErrors
                setTextRendererText (cesCodeEditors . ix codeInFile . cedErrorRenderer) allErrors
            Just (Right compiledValue) -> do
                -- Clear the error renderer
                setTextRendererText (cesCodeEditors . ix codeInFile . cedErrorRenderer) ""

                -- Cache the compiled value for use by new objects using this same codeInFile
                cesCodeEditors . ix codeInFile . cedCompiledValue ?= compiledValue
                
                -- Pass the compiled value to each registered "dependent" of the code editor
                dependents <- use (cesCodeEditors . ix codeInFile . cedDependents)
                lift $ forM_ dependents ($ compiledValue)


raycastCursor :: (MonadIO m, MonadState ECS m) => EntityID -> m Bool
raycastCursor handEntityID = fmap (fromMaybe False) $ runMaybeT $ do
    -- First, see if we can place a cursor into a text buffer.
    -- If not, then move onto the selection logic.
    selectedEntityID <- MaybeT $ viewSystem sysSelection selSelectedEntityID
    codeInFile       <- MaybeT $ getEntityComponent selectedEntityID myOnStartExpr
    editor           <- MaybeT $ viewSystem sysCodeEditor (cesCodeEditors . at codeInFile)
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
        cesCodeEditors . ix codeInFile . cedCodeRenderer .= updatedRenderer
    
    return True
