{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.EditorFrame where
import PreludeExtra

import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Constraint
import Rumpus.Systems.SceneEditor

addEditorFrame :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
addEditorFrame entityID = do
    editorFrame <- spawnEntity $ do
        removeComponent myShapeType
        myConstraint ==> RelativePositionTo entityID 0
    
    ------------------------
    -- Define a color editor
    color <- getEntityColor entityID
    _colorEditor <- spawnEntity $ do
        myParent            ==> editorFrame
        myShapeType         ==> SphereShape
        myColor             ==> color
        mySize              ==> 0.1
        myPhysicsProperties ==> [Kinematic, NoContactResponse]
        myConstraint        ==> RelativePositionTo editorFrame (V3 (-0.5) 0.5 0)
        --myPose              ==> (newPose & posPosition .~ V3 (-0.5) 0.5 0)
        myOnDrag            ==> \dragDistance -> do
            let x = dragDistance ^. _x
                newColor = hslColor (mod' x 1) 0.9 0.6
            setColor newColor
            setEntityColor newColor entityID

    -----------------------
    -- Define a size editor
    
    _sizeEditor <- spawnEntity $ do
        myParent            ==> editorFrame
        myShapeType         ==> CubeShape
        myColor             ==> V4 0.3 0.3 1 1
        mySize              ==> 0.2
        myPhysicsProperties ==> [Kinematic, NoContactResponse]
        myConstraint        ==> RelativePositionTo editorFrame (V3 0.5 0.5 0)
        --myPose              ==> (newPose & posPosition .~ V3 0.5 0.5 0)
        myOnDrag            ==> \dragDistance -> do
            let size = max 0.05 (abs dragDistance)
            -- Set the edited entity's size, not the editor-widget's : )
            setEntitySize size entityID

    modifySystemState sysSceneEditor $ 
        sedCurrentEditorFrame ?= editorFrame 