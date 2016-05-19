{-# LANGUAGE RankNTypes #-}
module Rumpus.Systems.CodeEditor where
import PreludeExtra hiding (Key)

import Graphics.GL.TextBuffer
import Halive.SubHalive
import Halive.Recompiler
import Halive.FileListener

import qualified Data.HashMap.Strict as Map
import Data.ECS.Vault

--import Rumpus.Systems.Collisions
import Rumpus.Systems.Shared
--import Rumpus.Systems.Hands
import Rumpus.Systems.Text
import Rumpus.Systems.Scene
import System.IO.Error
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

-- FIXME: these should move to their companion definitions (i.e. myStart, myUpdate etc.)
-- and those files should depend on CodeEditor and call registerCodeExprComponent.
-- Must thus be initialized after CodeEditor.
defineComponentKeyWithType "StartExpr"          [t|CodeInFile|]
defineComponentKeyWithType "UpdateExpr"         [t|CodeInFile|]
defineComponentKeyWithType "CodeHidden"         [t|Bool|]
--defineComponentKeyWithType "CollidingExpr"      [t|CodeInFile|]
--defineComponentKeyWithType "CollisionStartExpr" [t|CodeInFile|]

getEntityCodeHidden :: (MonadState ECS m) => EntityID -> m Bool
getEntityCodeHidden entityID = fromMaybe False <$> getEntityComponent entityID myCodeHidden

-- When in release mode, use the embedded "packages" directory,
-- otherwise use MSYS2's copy in /usr/local/ghc
sharedGHCSessionConfig :: GHCSessionConfig
sharedGHCSessionConfig = defaultGHCSessionConfig
    { gscFixDebounce = DebounceFix
    , gscStartupFile = Just ("resources"</>"Loader.hs", "start")
    --, gscCompilationMode = Compiled
    }

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

initCodeEditorSystem :: (MonadIO m, MonadState ECS m) => TChan CompilationRequest -> m ()
initCodeEditorSystem ghcChan = do

    registerSystem sysCodeEditor $ CodeEditorSystem
        { _cesCodeEditors = mempty
        , _cesGHCChan     = ghcChan
        }

    registerComponent         "CodeHidden"           myCodeHidden           (newComponentInterface myCodeHidden)
    registerCodeExprComponent "StartExpr"          myStartExpr          myStart
    registerCodeExprComponent "UpdateExpr"         myUpdateExpr         myUpdate
    --registerCodeExprComponent "CollidingExpr"      myCollidingExpr      myColliding
    --registerCodeExprComponent "CollisionStartExpr" myCollisionStartExpr myCollisionStart


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
registerWithCodeEditor codeInFile realCodeKey = do
    viewSystem sysCodeEditor (cesCodeEditors . at codeInFile) >>= \case
        Just existingEditor -> do
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
            forM_ (getCompiledValue newValue) $ \newCode ->
                setEntityComponent realCodeKey newCode entityID
            --putStrLnIO $ "Done setting code  " ++ show codeInFile ++ " on entity: " ++ show entityID
    modifySystemState sysCodeEditor $
        cesCodeEditors . at codeInFile . traverse . cedDependents . at entityID ?= updateCodeAction


createCodeEditor :: (MonadIO m, MonadState ECS m)
                 => CodeInFile -> m CodeEditor
createCodeEditor (codeFile, codeExpr) = do
    ghcChan         <- viewSystem sysCodeEditor cesGHCChan
    font            <- getFont
    codeFileInScene <- fileInScene codeFile

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

    codeFileInScene <- fileInScene codeFile

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

moveCodeEditorFile :: (MonadIO m, MonadState ECS m) => FilePath -> FilePath -> String -> m ()
moveCodeEditorFile oldFileName newFileName codeExpr = do
    let oldCodeInFile = (oldFileName, codeExpr)
        newCodeInFile = (newFileName, codeExpr)
    oldFileInScene <- fileInScene oldFileName
    newFileInScene <- fileInScene newFileName
    successful <- liftIO $ tryIOError $ renameFile oldFileInScene newFileInScene
    case successful of
        Left moveError -> do
            setErrorTextForCodeInFile oldCodeInFile (show moveError)
            putStrLnIO $ "In moveCodeEditorFile: " ++ show moveError
        Right _ -> modifySystemState sysCodeEditor $ do
            renameTextRendererFile newFileInScene (cesCodeEditors . ix oldCodeInFile . cedCodeRenderer)

            ghcChan <- use cesGHCChan
            mOldCodeEditor <- use (cesCodeEditors . at oldCodeInFile)
            -- Should never be missing, but we handle it anyway
            case mOldCodeEditor of
                Just oldCodeEditor -> do
                    let oldRecompiler = oldCodeEditor ^. cedRecompiler
                    newRecompiler <- renameRecompilerForExpression oldRecompiler ghcChan newFileInScene codeExpr
                    let newCodeEditor = oldCodeEditor & cedRecompiler .~ newRecompiler
                    cesCodeEditors . at oldCodeInFile .= Nothing
                    cesCodeEditors . at newCodeInFile ?= newCodeEditor
                Nothing            -> do
                    codeEditor <- lift $ createCodeEditor newCodeInFile
                    cesCodeEditors . at newCodeInFile ?= codeEditor


