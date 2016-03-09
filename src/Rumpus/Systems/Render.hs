{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Rumpus.Systems.Render where
import PreludeExtra

import qualified Data.Map as Map
import Rumpus.Systems.Shared
import Rumpus.Systems.Selection
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Controls
import Rumpus.Systems.Hands

import TinyRick

data Uniforms = Uniforms
    { uModelViewProjection :: UniformLocation (M44 GLfloat)
    , uModel               :: UniformLocation (M44 GLfloat)
    , uCamera              :: UniformLocation (V3  GLfloat)
    , uDiffuse             :: UniformLocation (V4  GLfloat)
    } deriving (Data)

data RenderSystem = RenderSystem 
    { _rdsShapes :: ![(ShapeType, Shape Uniforms)]
    }
makeLenses ''RenderSystem
defineSystemKey ''RenderSystem



initRenderSystem :: (MonadIO m, MonadState ECS m) => m ()
initRenderSystem = do
    glEnable GL_DEPTH_TEST
    glClearColor 0 0 0.1 1

    basicProg   <- createShaderProgram "resources/shaders/default.vert" "resources/shaders/default.frag"

    cubeGeo     <- cubeGeometry (V3 1 1 1) 1
    sphereGeo   <- icosahedronGeometry 1 5 -- radius subdivisions
    planeGeo    <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 1
    
    planeShape  <- makeShape planeGeo  basicProg
    cubeShape   <- makeShape cubeGeo   basicProg
    sphereShape <- makeShape sphereGeo basicProg

    let shapes = [(CubeShape, cubeShape), (SphereShape, sphereShape), (StaticPlaneShape, planeShape)]

    registerSystem sysRender (RenderSystem shapes)


tickRenderSystem :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m ()
tickRenderSystem headM44 = do
    vrPal  <- viewSystem sysControls ctsVRPal
    player <- viewSystem sysControls ctsPlayer
    -- Render the scene
    renderWith vrPal player headM44
        (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
        (\projM44 viewM44 -> do
            let projViewM44 = projM44 !*! viewM44
            renderEntities projViewM44
            renderEditors projViewM44
            )


renderEditors :: (MonadState ECS m, MonadIO m) => M44 GLfloat -> m ()
renderEditors projViewM44  = do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    traverseM_ (viewSystem sysSelection selSelectedEntityID) $ \entityID -> do
        parentPose <- getEntityPose entityID

        
        traverseM_ (getEntityComponent entityID cmpOnUpdateExpr) $ \codeExprKey -> 
            traverseM_ (viewSystem sysCodeEditor (cesCodeEditors . at codeExprKey)) $ \editor -> do

                let codeModelM44 = parentPose

                -- Render code in white
                renderText (editor ^. cedCodeRenderer) (projViewM44 !*! codeModelM44) (V3 1 1 1)

                let errorsModelM44 = codeModelM44 !*! identity & translation .~ V3 1 0 0

                -- Render errors in light red
                renderText (editor ^. cedErrorRenderer) (projViewM44 !*! errorsModelM44) (V3 1 0.5 0.5)

    glDisable GL_BLEND


renderEntities :: (MonadIO m, MonadState ECS m) 
               => M44 GLfloat -> m ()
renderEntities projViewM44 = do
    
    headM44 <- getHeadPose

    shapes <- viewSystem sysRender rdsShapes
    forM_ shapes $ \(shapeType, shape) -> withShape shape $ do

        Uniforms{..} <- asks sUniforms
        uniformV3 uCamera (headM44 ^. translation)

        -- Batch by entities sharing the same shape type
        entityIDsForShape <- getEntityIDsForShapeType shapeType
        forM_ entityIDsForShape $ \entityID -> do

            color <- getEntityColor entityID

            model <- getEntityTotalModelMatrix entityID
            uniformM44 uModelViewProjection (projViewM44 !*! model)
            uniformM44 uModel               model
            uniformV4  uDiffuse             color

            drawShape

-- | Accumulate a matrix stack by walking up to the parent
getEntityTotalModelMatrix :: MonadState ECS m => EntityID -> m (M44 GLfloat)
getEntityTotalModelMatrix startEntityID = do
    
    let go entityID = do
            pose   <- getEntityPose entityID
            size   <- getEntitySize entityID
            let !model = pose !*! scaleMatrix size

            inheritParent <- getEntityInheritParentTransform entityID
            if inheritParent 
                then do
                    getEntityComponent entityID cmpParent >>= \case
                        Just parent -> (!*! model) <$!> go parent
                        Nothing -> return model
                else return model
    
    go startEntityID

getEntityIDsForShapeType :: MonadState ECS m => ShapeType -> m [EntityID]
getEntityIDsForShapeType shapeType = Map.keys . Map.filter (== shapeType) <$> getComponentMap cmpShapeType
