module Rumpus.Systems.CodeEditorInput where
import PreludeExtra

import Graphics.GL.TextBuffer
import Halive.Recompiler
import Halive.FileListener

import qualified Data.HashMap.Strict as Map
import Control.Monad.Trans.Maybe

import Rumpus.Systems.Shared
import Rumpus.Systems.Controls
import Rumpus.Systems.Selection
import Rumpus.Systems.CodeEditor

import qualified Data.Sequence as Seq

-- | Passes keyboard events to the active code editor
tickCodeEditorInputSystem :: (MonadIO m, MonadState ECS m) => m ()
tickCodeEditorInputSystem = withSystem_ sysControls $ \ControlsSystem{..} -> do
    let events = _ctsEvents
        window = gpWindow _ctsVRPal

    modifySystemState sysCodeEditor $
        traverseM_ (Map.keys <$> use cesCodeEditors) $ \codeInFile -> do
            -- Ensure the buffers have the latest code text from disk
            refreshTextRendererFromFile (cesCodeEditors . ix codeInFile . cedCodeRenderer)

    mSelectedEntityID <- viewSystem sysSelection selSelectedEntityID
    forM mSelectedEntityID $ \selectedEntityID ->
        withEntityComponent selectedEntityID myStartExpr $ \codeInFile -> do
            didSave <- modifySystemState sysCodeEditor $
                fmap or . forM events $ \case
                    GLFWEvent e -> do
                        -- Make sure our events don't trigger reloading/recompilation
                        let causesSave = eventWillSaveTextBuffer e
                        when causesSave $
                            pauseFileWatchers codeInFile
                        handleTextBufferEvent window e
                            (cesCodeEditors . ix codeInFile . cedCodeRenderer)
                        return causesSave
                    _ -> return False
            when didSave $ do

                -- Disabling til I have energy to debug this
                wasModuleNameChange <- checkForModuleNameChange codeInFile
                unless wasModuleNameChange $
                    recompileCodeInFile codeInFile

pauseFileWatchers :: (MonadIO m, MonadState CodeEditorSystem m) => CodeInFile -> m ()
pauseFileWatchers codeInFile = useTraverseM_ (cesCodeEditors . at codeInFile) $ \codeEditor -> do
    setIgnoreTimeNow (codeEditor ^. cedRecompiler . to recFileEventListener)
    forM_ (codeEditor ^. cedCodeRenderer . txrFileEventListener) setIgnoreTimeNow


raycastCursor :: (MonadIO m, MonadState ECS m) => EntityID -> m Bool
raycastCursor handEntityID = fmap (fromMaybe False) $ runMaybeT $ do
    -- First, see if we can place a cursor into a text buffer.
    -- If not, then move onto the selection logic.
    selectedEntityID <- MaybeT $ viewSystem sysSelection selSelectedEntityID
    codeInFile       <- MaybeT $ getEntityComponent selectedEntityID myStartExpr
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

------------------
-- Module renaming

modulePrefix :: Seq Char
modulePrefix = Seq.fromList "module "
modulePrefixLen :: Int
modulePrefixLen = Seq.length modulePrefix

getModuleName :: TextSeq -> Maybe String
getModuleName textSeq = maybeModuleName
  where
    maybeModuleName = do
        moduleLine <- getModuleLine textSeq
        let moduleName = getModuleNameInLine moduleLine
        guard (not (Seq.null moduleName))
        return (toList moduleName)
    getModuleNameInLine = Seq.takeWhileL (/= ' ') . Seq.drop modulePrefixLen

    -- Find the first line beginning with 'module '
    getModuleLine = seqHead . Seq.filter isModuleLine . Seq.filter isPotentialModuleLine
      where
        -- Do a quick first pass for lines beginning with m before looking for the full module prefix
        isPotentialModuleLine = (== Just 'm') . seqHead
        isModuleLine          = (== modulePrefix) . Seq.take modulePrefixLen

getChangedModuleName :: TextBuffer -> Maybe String
getChangedModuleName textBuffer = do
    moduleName <- getModuleName (bufText textBuffer)
    guard ((takeBaseName <$> bufPath textBuffer) /= Just moduleName)
    return moduleName

-- | A facility to rename files and objects based on their module name,
-- to avoid introducing any extra interface for naming things.
-- Tricky as we must handle all the file watchers associated with the file.
checkForModuleNameChange :: (MonadState ECS m, MonadIO m)
                         => CodeInFile -> m Bool
checkForModuleNameChange codeInFile = do
    mBuffer <- viewSystemP sysCodeEditor
        (cesCodeEditors . ix codeInFile . cedCodeRenderer . txrTextBuffer)
    case mBuffer of
        Nothing -> return False
        Just textBuffer -> do
            let maybeNewModuleName = getChangedModuleName textBuffer
            case (maybeNewModuleName, bufPath textBuffer) of
                (Just newModuleName, Just oldFilePath) -> do
                    putStrLnIO $ "Module name changed: " ++ show (oldFilePath, newModuleName)
                    let oldFileName = takeFileName oldFilePath
                    moveCodeEditorFile oldFileName (newModuleName <.> "hs") "start"
                    return True
                _ -> return False
