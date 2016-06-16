{-# LANGUAGE RankNTypes #-}
module Rumpus.Systems.CodeEditor where
import PreludeExtra hiding (Key, catch)

import Graphics.GL.TextBuffer
import Halive.SubHalive
import Halive.Recompiler
import Halive.FileListener

import qualified Data.HashMap.Strict as Map
import Data.ECS.Vault

import Rumpus.Systems.Shared
import Rumpus.Systems.SceneWatcher
import Rumpus.Systems.Text
import Rumpus.Systems.Scene
import System.IO.Error

import Control.Monad.Trans.Control
import System.Timeout.Lifted
import Control.Exception.Lifted

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
    --, gscCompilationMode = Compiled
    }

rumpusLanguageExtensions :: [ExtensionFlag]
rumpusLanguageExtensions =
    [ Opt_FlexibleContexts
    , Opt_RecordWildCards
    , Opt_ViewPatterns
    , Opt_LambdaCase
    , Opt_MultiWayIf
    , Opt_BangPatterns
    , Opt_NegativeLiterals
    , Opt_TemplateHaskell
    , Opt_ScopedTypeVariables
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

initCodeEditorSystem :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m) => TChan CompilationRequest -> m ()
initCodeEditorSystem ghcChan = do

    registerSystem sysCodeEditor $ CodeEditorSystem
        { _cesCodeEditors = mempty
        , _cesGHCChan     = ghcChan
        }

    registerComponent         "CodeHidden"           myCodeHidden           (newComponentInterface myCodeHidden)
    registerCodeExprComponent "StartExpr"            myStartExpr            myStart
    registerCodeExprComponent "UpdateExpr"           myUpdateExpr           myUpdate
    --registerCodeExprComponent "CollidingExpr"      myCollisionContinuesExpr      myCollisionContinues
    --registerCodeExprComponent "CollisionBeganExpr" myCollisionBeganExpr myCollisionBegan


registerCodeExprComponent :: (MonadBaseControl IO m, MonadState ECS m, Typeable a)
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

registerWithCodeEditor :: (Typeable a, MonadBaseControl IO m, MonadIO m, MonadState ECS m, MonadReader EntityID m)
                       => CodeInFile
                       -> Key (EntityMap a)
                       -> m ()
registerWithCodeEditor codeInFile realCodeKey = do
    viewSystem sysCodeEditor (cesCodeEditors . at codeInFile) >>= \case
        Just existingEditor -> do
            -- FIXME: see below note in addCodeEditorDependency
            -- regarding the use of runUserFunctionProtected here
            runUserFunctionProtected realCodeKey $ do
                forM_ (existingEditor ^. cedCompiledValue) $ \compiledValue -> do
                    forM_ (getCompiledValue compiledValue)
                        (setComponent realCodeKey)
        Nothing -> do
            codeEditor <- createCodeEditor codeInFile
            modifySystemState sysCodeEditor $
                cesCodeEditors . at codeInFile ?= codeEditor
    addCodeEditorDependency codeInFile realCodeKey

addCodeEditorDependency :: (MonadState ECS m, MonadReader EntityID m, Typeable a)
                        => CodeInFile -> Key (EntityMap a) -> m ()
addCodeEditorDependency codeInFile realCodeKey = do
    entityID <- ask
    let updateCodeAction newValue = do
            --putStrLnIO $ "Setting code  " ++ show codeInFile ++ " on entity: " ++ show entityID
            forM_ (getCompiledValue newValue) $ \newCode -> do
                inEntity entityID $ do
                    -- FIXME: investigate this. The mere assignment of new code seems to
                    -- somehow run the code inside of it (???) so we have to run the
                    -- assignment in runUserFunctionProtected
                    runUserFunctionProtected realCodeKey $ do
                        realCodeKey ==> newCode
            --putStrLnIO $ "Done setting code  " ++ show codeInFile ++ " on entity: " ++ show entityID
    modifySystemState sysCodeEditor $
        cesCodeEditors . at codeInFile . traverse . cedDependents . at entityID ?= updateCodeAction

createCodeEditor :: (MonadIO m, MonadState ECS m)
                 => CodeInFile -> m CodeEditor
createCodeEditor (codeFile, codeExpr) = do
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
            }

unregisterWithCodeEditor :: (MonadReader EntityID m, MonadState ECS m) => CodeInFile -> m ()
unregisterWithCodeEditor codeInFile = do
    entityID <- ask
    modifySystemState sysCodeEditor $ do
        unregisterEntityWithCodeEditor entityID codeInFile

unregisterEntityWithCodeEditor :: (MonadState CodeEditorSystem m)
                               => EntityID -> CodeInFile -> m ()
unregisterEntityWithCodeEditor entityID codeInFile =
    cesCodeEditors . ix codeInFile . cedDependents . at entityID .= Nothing

withCodeEditor :: MonadState ECS m => (FilePath, String) -> (CodeEditor -> m b) -> m ()
withCodeEditor codeInFile =
    traverseM_ (viewSystem sysCodeEditor (cesCodeEditors . at codeInFile))

recompileCodeInFile :: (MonadIO m, MonadState ECS m)
                    => CodeInFile -> m ()
recompileCodeInFile codeInFile@(codeFile, codeExpr) = withCodeEditor codeInFile $ \codeEditor -> do
    ghcChan <- viewSystem sysCodeEditor cesGHCChan

    codeFileInScene <- fileInCurrentScene codeFile

    let resultsChan = codeEditor ^. cedRecompiler . to recResultTChan
        textBuffer  = codeEditor ^. cedCodeRenderer . txrTextBuffer
        compilationRequest = CompilationRequest
            { crResultTChan      = resultsChan
            -- NOTE: we want to make sure this isn't
            -- evaluated on this thread. Let the SubHalive thread do it:
            , crFileContents     = Just (stringFromTextBuffer textBuffer)
            , crFilePath         = codeFileInScene
            , crExpressionString = codeExpr
            }

    writeTChanIO ghcChan compilationRequest


-- | Allows Script system to pass runtime exceptions to the error pane,
-- assuming the given entityID has an onStartExpr.
setEntityErrorText :: (MonadIO m, MonadState ECS m) => EntityID -> String -> m ()
setEntityErrorText entityID errorText = do
    traverseM_ (getEntityComponent entityID myStartExpr) $ \codeInFile ->
        setErrorTextForCodeInFile codeInFile errorText

setErrorText :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => String -> m ()
setErrorText errorText = do
    entityID <- ask
    setEntityErrorText entityID errorText

setErrorTextForCodeInFile :: (MonadIO m, MonadState ECS m) => CodeInFile -> String -> m ()
setErrorTextForCodeInFile codeInFile errorText = modifySystemState sysCodeEditor $
    setTextRendererText (cesCodeEditors . ix codeInFile . cedErrorRenderer) errorText


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

moveCodeEditorFile :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m) => FilePath -> FilePath -> String -> m ()
moveCodeEditorFile oldFileName newFileName codeExpr = do
    let oldCodeInFile = (oldFileName, codeExpr)
        newCodeInFile = (newFileName, codeExpr)
    oldFilePath <- fileInCurrentScene oldFileName
    newFilePath <- fileInCurrentScene newFileName
    successful <- liftIO $ tryIOError $ renameFile oldFilePath newFilePath
    case successful of
        Left moveError -> do
            setErrorTextForCodeInFile oldCodeInFile (show moveError)
            putStrLnIO $ "In moveCodeEditorFile: " ++ show moveError
        Right _ -> do
            mOldCodeEditor <- viewSystem sysCodeEditor (cesCodeEditors . at oldCodeInFile)
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
                        cesCodeEditors . at oldCodeInFile .= Nothing
                        cesCodeEditors . at newCodeInFile ?= newCodeEditor

                    forM_ (Map.keys (oldCodeEditor ^. cedDependents)) $ \entityID -> do
                        inEntity entityID (setStartExpr newCodeInFile)
                        sceneWatcherSaveEntity entityID
                Nothing            -> do
                    putStrLnIO $
                        "moveCodeEditorFile couldn't find codeEditor for " ++ show oldCodeInFile


setStartExpr :: (MonadIO m, MonadBaseControl IO m, MonadState ECS m, MonadReader EntityID m)
             => CodeInFile -> m ()
setStartExpr codeInFile = do
    myStartExpr ==> codeInFile
    registerWithCodeEditor codeInFile myStart

spawnChildInstance :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m, MonadReader EntityID m) => FilePath -> m EntityID
spawnChildInstance codeFileName = do
    let codePath = codeFileName <.> "hs"
    spawnChild $ do
        myStartExpr   ==> (codePath, "start")
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
