{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.EditorFrame where
import PreludeExtra

import Rumpus.Systems.Shared
import Rumpus.Systems.Drag
import Rumpus.Systems.Physics
import Rumpus.Systems.Constraint
import Rumpus.Systems.SceneEditor

addEditorFrame :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
addEditorFrame entityID = do
    editorFrame <- spawnEntity $ do
        myConstraint ==> RelativePositionTo entityID 0

    _ <- runEntity editorFrame $ do
        ------------------------
        -- Define a color editor
        color <- getEntityColor entityID
        spawnChild $ do
            myShape      ==> Sphere
            myColor      ==> color
            mySize       ==> 0.1
            myProperties ==> [Floating, Ghostly]
            myConstraint ==> RelativePositionTo editorFrame (V3 (-0.5) 0.5 0)
            myDrag       ==> \changeM44 -> do
                let x = changeM44 ^. translation . _x
                    newColor = colorHSL (mod' x 1) 0.9 0.6
                setColor newColor
                setEntityColor newColor entityID
        -----------------------
        -- Define a size editor
        spawnChild $ do
            myShape      ==> Cube
            myColor      ==> V4 0.3 0.3 1 1
            mySize       ==> 0.2
            myProperties ==> [Floating, Ghostly]
            myConstraint ==> RelativePositionTo editorFrame (V3 0.5 0.5 0)
            myDrag       ==> \changeM44 -> do
                let size = max 0.05 (abs $ changeM44 ^. translation)
                -- Set the edited entity's size, not the editor-widget's : )
                setEntitySize size entityID

    modifySystemState sysSceneEditor $
        sedCurrentEditorFrame ?= editorFrame
