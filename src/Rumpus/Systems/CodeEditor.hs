{-# LANGUAGE RankNTypes #-}
module Rumpus.Systems.CodeEditor where
import PreludeExtra hiding (Key, catch)

import Graphics.GL.TextBuffer
import Halive.SubHalive
import Halive.Recompiler
import Halive.FileListener

import qualified Data.HashMap.Strict as Map
import Data.ECS.Vault

import Control.Monad.Trans.Maybe

import Rumpus.Systems.Shared
import Rumpus.Systems.SceneWatcher
import Rumpus.Systems.Text
import Rumpus.Systems.Scene
import System.IO.Error
import System.Timeout.Lifted
import Control.Exception.Lifted

data CodeEditor = CodeEditor
    { _cedRecompiler    :: !Recompiler
    , _cedCompiledValue :: !(Maybe CompiledValue)
    , _cedCodeRenderer  :: !TextRenderer
    , _cedErrorRenderer :: !TextRenderer
    , _cedHasResult     :: !Bool
    , _cedDependents    :: !(Map EntityID (CompiledValue -> ECSMonad ()))
    }
makeLenses ''CodeEditor

data CodeEditorSystem = CodeEditorSystem
    { _cesCodeEditors :: !(Map SceneCodeFile CodeEditor)
    , _cesGHCChan     :: !(TChan CompilationRequest)
    }
makeLenses ''CodeEditorSystem
defineSystemKey ''CodeEditorSystem

getGHCChan :: MonadState ECS m => m (TChan CompilationRequest)
getGHCChan = viewSystem sysCodeEditor cesGHCChan

getEntityCodeHidden :: (MonadState ECS m) => EntityID -> m Bool
getEntityCodeHidden entityID = fromMaybe False <$> getEntityComponent entityID myCodeHidden

-- When in release mode, use the embedded "packages" directory,
-- otherwise use MSYS2's copy in /usr/local/ghc
sharedGHCSessionConfig :: GHCSessionConfig
sharedGHCSessionConfig = defaultGHCSessionConfig
    { gscFixDebounce = DebounceFix
    , gscStartupFile = Just ("resources"</>"Loader.hs", "start")
    , gscLanguageExtensions = rumpusLanguageExtensions
    --, gscVerbosity = 3
    --, gscCompilationMode = Compiled
    }

rumpusLanguageExtensions :: [Extension]
rumpusLanguageExtensions =
    [ FlexibleContexts
    , RecordWildCards
    , ViewPatterns
    , LambdaCase
    , MultiWayIf
    , BangPatterns
    , NegativeLiterals
    , TemplateHaskell
    , ScopedTypeVariables
    ]


withRumpusGHC :: MonadIO m => (TChan CompilationRequest -> m a) -> m a
withRumpusGHC action = do
    useEmbeddedPackages <- liftIO (doesDirectoryExist "packages")
    let config = if useEmbeddedPackages
            then sharedGHCSessionConfig
                { gscPackageDBs = [ "packages"</>"local"</>"pkgdb"
                                  , "packages"</>"snapshot"</>"pkgdb"
                                  ]
                , gscLibDir     =   "packages"</>"ghc"</>"lib"
                }
            else sharedGHCSessionConfig
    withGHC config action

initCodeEditorSystem :: (MonadBaseControl IO m, MonadState ECS m)
                     => TChan CompilationRequest -> m ()
initCodeEditorSystem ghcChan = do

    registerSystem sysCodeEditor $ CodeEditorSystem
        { _cesCodeEditors = mempty
        , _cesGHCChan     = ghcChan
        }

    registerComponent
        "CodeHidden"
        myCodeHidden
        (newComponentInterface myCodeHidden)
    registerCodeExprComponent
        "StartCodeFile" myStartCodeFile
        myStart

registerCodeExprComponent :: (MonadState ECS m, Typeable a)
                          => String
                          -> Key (EntityMap CodeFile)
                          -> Key (EntityMap a)
                          -> m ()
registerCodeExprComponent codeFileKeyName codeFileKey realCodeKey =
    registerComponent codeFileKeyName codeFileKey
        $ (savedComponentInterface codeFileKey)
            { ciDeriveComponent  = Just (
                withComponent_ codeFileKey $ \codeFile -> do
                    registerWithCodeEditor codeFile realCodeKey
                )
            , ciRemoveComponent = do
                withComponent_  codeFileKey unregisterWithCodeEditor
                removeComponent codeFileKey
            }

toSceneCodeFile :: MonadState ECS m => CodeFile -> m SceneCodeFile
toSceneCodeFile (file, expr) = do
    sceneName <- getSceneName
    return (sceneName, file, expr)

-- | Finds the code editor for the given CodeFile
registerWithCodeEditor :: (Typeable a, MonadBaseControl IO m, MonadIO m, MonadState ECS m, MonadReader EntityID m)
                       => CodeFile
                       -> Key (EntityMap a)
                       -> m ()
registerWithCodeEditor codeFile realCodeKey = do
    sceneCodeFile <- toSceneCodeFile codeFile
    viewSystem sysCodeEditor (cesCodeEditors . at sceneCodeFile) >>= \case
        Just existingEditor -> do
            -- FIXME: see below note in addCodeEditorDependency
            -- regarding the use of runUserFunctionProtected here
            runUserFunctionProtected realCodeKey $ do
                forM_ (existingEditor ^. cedCompiledValue) $ \compiledValue -> do
                    forM_ (getCompiledValue compiledValue)
                        (setComponent realCodeKey)
        Nothing -> do
            codeEditor <- createCodeEditor sceneCodeFile
            modifySystemState sysCodeEditor $
                cesCodeEditors . at sceneCodeFile ?= codeEditor
    addCodeEditorDependency sceneCodeFile realCodeKey

addCodeEditorDependency :: (MonadState ECS m, MonadReader EntityID m, Typeable a)
                        => SceneCodeFile -> Key (EntityMap a) -> m ()
addCodeEditorDependency sceneCodeFile realCodeKey = do
    entityID <- ask
    let updateCodeAction newValue = do
            --putStrLnIO $ "Setting code  " ++ show sceneCodeFile ++ " on entity: " ++ show entityID
            forM_ (getCompiledValue newValue) $ \newCode -> do
                inEntity entityID $ do
                    -- FIXME: investigate this. The mere assignment of new code seems to
                    -- somehow run the code inside of it (???) so we have to run the
                    -- assignment in runUserFunctionProtected
                    runUserFunctionProtected realCodeKey $ do
                        realCodeKey ==> newCode
            --putStrLnIO $ "Done setting code  " ++ show sceneCodeFile ++ " on entity: " ++ show entityID
    modifySystemState sysCodeEditor $
        cesCodeEditors . at sceneCodeFile . traverse . cedDependents . at entityID ?= updateCodeAction

createCodeEditor :: (MonadIO m, MonadState ECS m)
                 => SceneCodeFile -> m CodeEditor
createCodeEditor (_, codeFile, codeExpr) = do
    ghcChan         <- getGHCChan
    font            <- getFont
    codeFileInScene <- fileInCurrentScene codeFile

    recompiler <- recompilerForExpression ghcChan codeFileInScene codeExpr True

    let setupRenderer r =
            updateMetrics ((txrScreenSize ?~ V2 80 80) r)
    -- FIXME this should be async...
    -- (note: could implement that using WatchFile/refreshText by writing a phony event)
    codeRenderer  <- setupRenderer =<< textRendererFromFile font codeFileInScene WatchFile
    errorRenderer <- setupRenderer =<< createTextRenderer font (textBufferFromString "")

    return CodeEditor
            { _cedCodeRenderer  = codeRenderer
            , _cedErrorRenderer = errorRenderer
            , _cedRecompiler    = recompiler
            , _cedCompiledValue = Nothing
            , _cedDependents    = mempty
            , _cedHasResult     = False
            }

unregisterWithCodeEditor :: (MonadReader EntityID m, MonadState ECS m) => CodeFile -> m ()
unregisterWithCodeEditor codeFile = do
    entityID <- ask
    sceneCodeFile <- toSceneCodeFile codeFile
    modifySystemState sysCodeEditor $ do
        unregisterEntityWithCodeEditor entityID sceneCodeFile

unregisterEntityWithCodeEditor :: (MonadState CodeEditorSystem m)
                               => EntityID -> SceneCodeFile -> m ()
unregisterEntityWithCodeEditor entityID sceneCodeFile = do
    cesCodeEditors . ix sceneCodeFile . cedDependents . at entityID .= Nothing

withCodeEditor :: MonadState ECS m
               => SceneCodeFile -> (CodeEditor -> m b) -> m ()
withCodeEditor sceneCodeFile =
    traverseM_
        (viewSystem sysCodeEditor
            (cesCodeEditors . at sceneCodeFile))

recompileCodeFile :: (MonadIO m, MonadState ECS m)
                  => SceneCodeFile -> m ()
recompileCodeFile sceneCodeFile@(_, file, expr) =
    withCodeEditor sceneCodeFile $ \codeEditor -> do
        ghcChan <- viewSystem sysCodeEditor cesGHCChan

        codeFileInScene <- fileInCurrentScene file

        let resultsChan = codeEditor ^. cedRecompiler . to recResultTChan
            textBuffer  = codeEditor ^. cedCodeRenderer . txrTextBuffer
            compilationRequest = CompilationRequest
                { crResultTChan      = resultsChan
                -- NOTE: we want to make sure this isn't
                -- evaluated on this thread. Let the SubHalive thread do it:
                , crFileContents     = Just (stringFromTextBuffer textBuffer)
                , crFilePath         = codeFileInScene
                , crExpressionString = expr
                }

        writeTChanIO ghcChan compilationRequest


-- | Allows Script system to pass runtime exceptions to the error pane,
-- assuming the given entityID has an onStartCodeFile.
setEntityErrorText :: (MonadIO m, MonadState ECS m) => EntityID -> String -> m ()
setEntityErrorText entityID errorText = do
    traverseM_ (getEntityComponent entityID myStartCodeFile) $ \codeFile ->
        setErrorTextForCodeFile codeFile errorText

setErrorText :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => String -> m ()
setErrorText errorText = do
    entityID <- ask
    setEntityErrorText entityID errorText

setErrorTextForCodeFile :: (MonadIO m, MonadState ECS m) => CodeFile -> String -> m ()
setErrorTextForCodeFile codeFile errorText = do
    sceneCodeFile <- toSceneCodeFile codeFile
    modifySystemState sysCodeEditor $
        setTextRendererText (cesCodeEditors . ix sceneCodeFile . cedErrorRenderer) errorText

getStartCodeEditor :: (MonadState ECS m, MonadReader EntityID m) => m (Maybe CodeEditor)
getStartCodeEditor = runMaybeT $ do
    codeFile <- MaybeT $ getComponent myStartCodeFile
    sceneCodeFile <- toSceneCodeFile codeFile
    codeEditor <- MaybeT $ viewSystem sysCodeEditor (cesCodeEditors . at sceneCodeFile)
    return codeEditor

-- | Update the world state with the result of the editor upon successful compilations
-- or update the error renderers for each code editor on failures
tickCodeEditorResultsSystem :: ECSMonad ()
tickCodeEditorResultsSystem = modifySystemState sysCodeEditor $
    traverseM_ (Map.toList <$> use cesCodeEditors) $ \(sceneCodeFile, editor) -> do

        -- Update entities with new code from the compiler
        tryReadTChanIO (editor ^. cedRecompiler . to recResultTChan) >>= \case
            Nothing -> return ()
            Just (Left errors) -> do
                cesCodeEditors . ix sceneCodeFile . cedHasResult .= True
                let allErrors = unlines errors
                putStrLnIO allErrors
                setTextRendererText (cesCodeEditors . ix sceneCodeFile . cedErrorRenderer) allErrors
            Just (Right compiledValue) -> do
                cesCodeEditors . ix sceneCodeFile . cedHasResult .= True
                -- Clear the error renderer
                setTextRendererText (cesCodeEditors . ix sceneCodeFile . cedErrorRenderer) ""

                -- Cache the compiled value for use by new objects using this same sceneCodeFile
                cesCodeEditors . ix sceneCodeFile . cedCompiledValue ?= compiledValue

                -- Pass the compiled value to each registered "dependent" of the code editor
                dependents <- use (cesCodeEditors . ix sceneCodeFile . cedDependents)
                lift $ forM_ dependents ($ compiledValue)

moveCodeEditorFile :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m) => FilePath -> FilePath -> String -> m ()
moveCodeEditorFile oldFileName newFileName codeExpr = do
    let newCodeFile = (newFileName, codeExpr)
        oldCodeFile = (oldFileName, codeExpr)
    oldSceneCodeFile <- toSceneCodeFile oldCodeFile
    newSceneCodeFile <- toSceneCodeFile newCodeFile
    oldFilePath <- fileInCurrentScene oldFileName
    newFilePath <- fileInCurrentScene newFileName
    successful <- liftIO $ tryIOError $ renameFile oldFilePath newFilePath
    case successful of
        Left moveError -> do
            setErrorTextForCodeFile oldCodeFile (show moveError)
            putStrLnIO $ "In moveCodeEditorFile: " ++ show moveError
        Right _ -> do
            mOldCodeEditor <- viewSystem sysCodeEditor (cesCodeEditors . at oldSceneCodeFile)
            case mOldCodeEditor of
                Just oldCodeEditor -> do
                    let oldCodeRenderer = oldCodeEditor ^. cedCodeRenderer
                    newCodeRenderer <- renameTextRendererFile newFilePath oldCodeRenderer

                    let oldRecompiler = oldCodeEditor ^. cedRecompiler
                    ghcChan       <- getGHCChan
                    newRecompiler <- renameRecompilerForExpression oldRecompiler ghcChan newFilePath codeExpr
                    let newCodeEditor = oldCodeEditor & cedRecompiler .~ newRecompiler
                                                      & cedCodeRenderer .~ newCodeRenderer
                    modifySystemState sysCodeEditor $ do
                        cesCodeEditors . at oldSceneCodeFile .= Nothing
                        cesCodeEditors . at newSceneCodeFile ?= newCodeEditor

                    forM_ (Map.keys (oldCodeEditor ^. cedDependents)) $ \entityID -> do
                        inEntity entityID (setStartCodeFile newCodeFile)
                        sceneWatcherSaveEntity entityID
                Nothing -> do
                    putStrLnIO $
                        "moveCodeEditorFile couldn't find codeEditor for " ++ show oldSceneCodeFile


setStartCodeFile :: (MonadIO m, MonadBaseControl IO m, MonadState ECS m, MonadReader EntityID m)
                 => CodeFile -> m ()
setStartCodeFile codeFile = do
    myStartCodeFile ==> codeFile
    registerWithCodeEditor codeFile myStart

spawnChildInstance :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m, MonadReader EntityID m) => FilePath -> m EntityID
spawnChildInstance codeFileName = do
    let codePath = codeFileName <.> "hs"
    spawnChild $ do
        myStartCodeFile   ==> (codePath, "start")
        myCodeHidden  ==> True




-- CODE PROTECTION

-- | We give scripts a generous 1 second
-- in case they want to generate a lot of geometry
-- SteamVR will pull the screen away for us so it won't be painful.
-- Timeout takes microseconds (1e6)
maxUserFunctionTime :: Int
maxUserFunctionTime = 1 * 10^(6::Int)

-- | Returns Just a
-- Should wrap each function call in runUserFunctionProtected
runUserScriptsWithTimeout :: MonadBaseControl IO m => m a -> m (Maybe a)
runUserScriptsWithTimeout = timeout maxUserFunctionTime

runUserScriptsWithTimeout_ :: MonadBaseControl IO m => m a -> m ()
runUserScriptsWithTimeout_ = void . runUserScriptsWithTimeout

-- | Should be called within a runUserScriptsWithTimeout
-- Handle any exceptions during a user function
-- by writing them to the error pane and clearing the function.
runUserFunctionProtected :: (MonadIO m, MonadBaseControl IO m, MonadState ECS m, MonadReader EntityID m)
                         => Key (EntityMap a) -> m () -> m ()
runUserFunctionProtected functionKey userFunction = do
    catch userFunction (\e -> do
        removeComponent functionKey
        let runtimeErrors = displayException (e::SomeException)
        setErrorText runtimeErrors
        putStrLnIO $ "runUserFunctionProtected caught: " ++ runtimeErrors)
