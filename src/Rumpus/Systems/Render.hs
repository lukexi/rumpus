{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Rumpus.Systems.Render where
import PreludeExtra

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Rumpus.Systems.Shared
import Rumpus.Systems.Selection
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Graphics.GL.TextBuffer

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

    --finalMatricesByEntityID <- profileMS "getFinalMatrices" 2 $ getFinalMatrices
    finalMatricesByEntityID <- getFinalMatrices
    -- Render the scene
    renderWith vrPal player headM44
        (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
        (\projM44 viewM44 -> do
            let projViewM44 = projM44 !*! viewM44
            renderEntities projViewM44 finalMatricesByEntityID
            renderEditors projViewM44
            )


renderEditors :: (MonadState ECS m, MonadIO m) => M44 GLfloat -> m ()
renderEditors projViewM44  = do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    entitiesWithOnStart <- Map.toList <$> getComponentMap cmpOnStartExpr
    forM_ entitiesWithOnStart $ \(entityID, codeExprKey) -> 
        traverseM_ (viewSystem sysCodeEditor (cesCodeEditors . at codeExprKey)) $ \editor -> do
            parentPose <- getEntityPose entityID

            let codeModelM44 = parentPose !*! (identity & translation .~ V3 0 0 (-0.2)) !*! scaleMatrix 0.2

            -- Render code in white
            renderText (editor ^. cedCodeRenderer) (projViewM44 !*! codeModelM44) (V3 1 1 1)

            let errorsModelM44 = codeModelM44 !*! identity & translation .~ V3 1 0 0

            -- Render errors in light red
            renderText (editor ^. cedErrorRenderer) (projViewM44 !*! errorsModelM44) (V3 1 0.5 0.5)

    glDisable GL_BLEND


renderEntities :: (MonadIO m, MonadState ECS m) 
               => M44 GLfloat -> Map EntityID (M44 GLfloat) -> m ()
renderEntities projViewM44 finalMatricesByEntityID = do
    
    headM44 <- getHeadPose

    shapes <- viewSystem sysRender rdsShapes
    forM_ shapes $ \(shapeType, shape) -> withShape shape $ do

        Uniforms{..} <- asks sUniforms
        uniformV3 uCamera (headM44 ^. translation)

        -- Batch by entities sharing the same shape type
        entityIDsForShape <- getEntityIDsForShapeType shapeType
        forM_ entityIDsForShape $ \entityID -> do

            color <- getEntityColor entityID

            --model <- getEntityTotalModelMatrix entityID
            let model = fromMaybe identity $ Map.lookup entityID finalMatricesByEntityID 
            uniformM44 uModelViewProjection (projViewM44 !*! model)
            uniformM44 uModel               model
            uniformV4  uDiffuse             color

            drawShape

-- Perform a breadth-first traversal of entities with no parents, 
-- accumulating their matrix mults all the way down into any children.
-- This avoids duplicate matrix multiplications.
getFinalMatrices :: MonadState ECS m => m (Map EntityID (M44 GLfloat))
getFinalMatrices = do
    entityIDs           <- Set.fromList . Map.keys <$> getComponentMap cmpPose
    entityIDsWithChild  <- Set.fromList . Map.keys <$> getComponentMap cmpChildren
    entityIDsWithParent <- Set.fromList . Map.keys <$> getComponentMap cmpParent

    let rootIDs = Set.union entityIDs entityIDsWithChild Set.\\ entityIDsWithParent
        go mParentMatrix accum entityID = do
            -- See if we want to inherit our parent's matrix
            inherit <- getEntityInheritParentTransform entityID
            -- 
            entityMatrixRaw <- getScaledMatrix entityID
            entityMatrix <- case (inherit, mParentMatrix) of
                (InheritFull, Just (_ ,parentMatrix)) -> return (parentMatrix !*! entityMatrixRaw)
                (InheritPose, Just (parentID, _))     -> do
                    parentPose <- getEntityPose parentID
                    return (parentPose !*! entityMatrixRaw) 
                _                                     -> return entityMatrixRaw

            -- Pass the calculated matrix down to each child so it can calculate its own final matrix
            children <- getEntityChildren entityID
            foldM (go (Just (entityID, entityMatrix))) (Map.insert entityID entityMatrix accum) children
    foldM (go Nothing) mempty rootIDs

getScaledMatrix :: MonadState ECS m => EntityID -> m (M44 GLfloat)
getScaledMatrix entityID = do
    pose <- getEntityPose entityID
    size <- getEntitySize entityID
    return $! pose !*! scaleMatrix size

getEntityIDsForShapeType :: MonadState ECS m => ShapeType -> m [EntityID]
getEntityIDsForShapeType shapeType = Map.keys . Map.filter (== shapeType) <$> getComponentMap cmpShapeType
