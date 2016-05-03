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
import Data.ECS.Vault

import Rumpus.Systems.Collisions
import Rumpus.Systems.Shared
--import Rumpus.Systems.Hands
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Text
import Rumpus.Systems.Scene
import Rumpus.Types

--import Data.Time.Clock.POSIX

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

defineComponentKeyWithType "StartExpr"          [t|CodeInFile|]
defineComponentKeyWithType "UpdateExpr"         [t|CodeInFile|]
defineComponentKeyWithType "CollidingExpr"      [t|CodeInFile|]
defineComponentKeyWithType "CollisionStartExpr" [t|CodeInFile|]

defineComponentKeyWithType "PlayWhenReady"      [t|Bool|]


-- When in release mode, use the embedded "packages" directory,
-- otherwise use MSYS2's copy in /usr/local/ghc
sharedGHCSessionConfig :: GHCSessionConfig
sharedGHCSessionConfig = defaultGHCSessionConfig
    { gscFixDebounce = DebounceFix
    --, gscStartupFile = "resources"</>"Loader.hs"
    --, gscCompilationMode = Compiled
    }

rumpusGHCSessionConfig :: GHCSessionConfig
rumpusGHCSessionConfig = if isInReleaseMode
    then sharedGHCSessionConfig
        { gscPackageDBs = [ "packages"</>"local"</>"pkgdb"
                          , "packages"</>"snapshot"</>"pkgdb"
                          ]
        , gscLibDir     =   "packages"</>"ghc"</>"lib"
        }
    else sharedGHCSessionConfig

getPlayWhenReady :: (MonadState ECS m, MonadReader EntityID m) => m Bool
getPlayWhenReady = fromMaybe False <$> getComponent myPlayWhenReady

getEntityPlayWhenReady :: (MonadState ECS m) => EntityID -> m Bool
getEntityPlayWhenReady entityID = fromMaybe False <$> getEntityComponent entityID myPlayWhenReady

-- This is used to implement a workaround
withGHC :: MonadIO m => GHCSessionConfig -> (TChan CompilationRequest -> m b) -> m b
withGHC ghcSessionConfig action = do
    ghcChan <- startGHC ghcSessionConfig
    action ghcChan

initCodeEditorSystem :: (MonadIO m, MonadState ECS m) => TChan CompilationRequest -> m ()
initCodeEditorSystem ghcChan = do

    registerSystem sysCodeEditor $ CodeEditorSystem
        { _cesCodeEditors = mempty
        , _cesGHCChan     = ghcChan
        }

    -- Will require (scriptPath, "start") (or "update" or "collision") to be added somewhere!
    registerCodeExprComponent "StartExpr"          myStartExpr          myStart
    registerCodeExprComponent "UpdateExpr"         myUpdateExpr         myUpdate
    registerCodeExprComponent "CollidingExpr"      myCollidingExpr      myColliding
    registerCodeExprComponent "CollisionStartExpr" myCollisionStartExpr myCollisionStart

    registerComponent "PlayWhenReady" myPlayWhenReady (savedComponentInterface myPlayWhenReady)


registerCodeExprComponent :: (MonadState ECS m, Typeable a)
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

registerWithCodeEditor :: (Typeable a, MonadIO m, MonadState ECS m, MonadReader EntityID m)
                       => CodeInFile
                       -> Key (EntityMap a)
                       -> m ()
registerWithCodeEditor codeInFile realCodeKey = modifySystemState sysCodeEditor $ do
    use (cesCodeEditors . at codeInFile) >>= \case
        Just existingEditor -> do
            forM_ (existingEditor ^. cedCompiledValue) $ \compiledValue -> do
                forM_ (getCompiledValue compiledValue)
                    (lift . setComponent realCodeKey)
        Nothing -> do
            codeEditor <- createCodeEditor codeInFile
            cesCodeEditors . at codeInFile ?= codeEditor
    addCodeEditorDependency codeInFile realCodeKey

addCodeEditorDependency :: (MonadState CodeEditorSystem m, MonadReader EntityID m, Typeable a)
                        => CodeInFile -> Key (EntityMap a) -> m ()
addCodeEditorDependency codeInFile realCodeKey = do
    entityID <- ask
    let updateCodeAction newValue = do
            putStrLnIO $ "Setting code  " ++ show codeInFile ++ " on entity: " ++ show entityID
            forM_ (getCompiledValue newValue) $ \newCode ->
                setEntityComponent realCodeKey newCode entityID
            putStrLnIO $ "Done setting code  " ++ show codeInFile ++ " on entity: " ++ show entityID
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

    recompiler <- recompilerForExpression ghcChan scriptFullPath exprString

    let setupRenderer r =
            updateMetrics ((txrScreenSize ?~ V2 80 80) r)
    -- FIXME this should be async...
    -- (note: could implement that using WatchFile/refreshText by writing a phony event)
    codeRenderer  <- setupRenderer =<< textRendererFromFile font scriptFullPath WatchFile
    errorRenderer <- setupRenderer =<< createTextRenderer font (textBufferFromString "")

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
    traverseM_ (getEntityComponent entityID myStartExpr) $ \codeInFile ->
        modifySystemState sysCodeEditor $
            setTextRendererText (cesCodeEditors . ix codeInFile . cedErrorRenderer) errors

-- | Update the world state with the result of the editor upon successful compilations
-- or update the error renderers for each code editor on failures
tickCodeEditorResultsSystem :: ECSMonad ()
tickCodeEditorResultsSystem = modifySystemState sysCodeEditor $
    traverseM_ (Map.toList <$> use cesCodeEditors) $ \(codeInFile, editor) -> do

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





-----------------------------------------------
-- Experiments in dynamic code addition/cloning
-----------------------------------------------
{-
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
    let codeFileComponentKey = myStartExpr
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
-}
--
