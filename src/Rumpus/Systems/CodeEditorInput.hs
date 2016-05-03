{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Rumpus.Systems.CodeEditorInput where
import PreludeExtra hiding (Key)

import Graphics.GL.TextBuffer
import Halive.Recompiler
import Halive.FileListener

import qualified Data.HashMap.Strict as Map
import Control.Monad.Trans.Maybe

import Rumpus.Systems.Shared


--import Rumpus.Systems.Hands
import Rumpus.Systems.Controls
import Rumpus.Systems.Selection
import Rumpus.Systems.CodeEditor
--import Rumpus.Systems.Collisions

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



-- | Passes keyboard events to the active code editor
tickCodeEditorInputSystem :: (MonadIO m, MonadState ECS m) => m ()
tickCodeEditorInputSystem = withSystem_ sysControls $ \ControlsSystem{..} -> do
    let events = _ctsEvents
        window = gpWindow _ctsVRPal

    modifySystemState sysCodeEditor $
        traverseM_ (Map.toList <$> use cesCodeEditors) $ \(codeInFile, _editor) -> do
            -- Ensure the buffers have the latest code text from disk
            refreshTextRendererFromFile (cesCodeEditors . ix codeInFile . cedCodeRenderer)

    mSelectedEntityID <- viewSystem sysSelection selSelectedEntityID
    forM mSelectedEntityID $ \selectedEntityID ->
        withEntityComponent selectedEntityID myStartExpr $ \codeInFile ->
            modifySystemState sysCodeEditor $ do

                didSave <- fmap or . forM events $ \case
                    GLFWEvent e -> do
                        -- Make sure our events don't trigger reloading/recompilation
                        -- (fixme: should be able to ask text buffer whether event will trigger save and only pause if so)
                        let causesSave = eventWillSaveTextBuffer e
                        when causesSave $
                            pauseFileWatchers codeInFile
                        handleTextBufferEvent window e
                            (cesCodeEditors . ix codeInFile . cedCodeRenderer)
                        return causesSave
                    _ -> return False
                when didSave $ do
                    recompileCodeInFile codeInFile


pauseFileWatchers :: (MonadIO m, MonadState CodeEditorSystem m) => CodeInFile -> m ()
pauseFileWatchers codeInFile = useTraverseM_ (cesCodeEditors . at codeInFile) $ \codeEditor -> do
    setIgnoreTimeNow (codeEditor ^. cedRecompiler . to recFileEventListener)
    forM_ (codeEditor ^. cedCodeRenderer . txrFileEventListener) setIgnoreTimeNow
