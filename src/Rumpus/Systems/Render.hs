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
import Rumpus.Systems.Text
import Rumpus.Types
import Graphics.GL.TextBuffer

import qualified Data.Vector.Storable.Mutable as VM

data Uniforms = Uniforms
    { uProjectionView :: UniformLocation (M44 GLfloat)
    , uCamera         :: UniformLocation (V3  GLfloat)
    } deriving (Data)

data RenderShape = RenderShape
    { rshShapeType                :: ShapeType
    , rshShape                    :: Shape Uniforms
    , rshInstanceColorsBuffer     :: ArrayBuffer --FIXME add a phantom type for these!!
    , rshInstanceColorsMVector    :: VM.IOVector (V4 GLfloat)
    , rshInstanceModelM44sBuffer  :: ArrayBuffer 
    , rshInstanceModelM44sMVector :: VM.IOVector (M44 GLfloat)
    }

data RenderSystem = RenderSystem 
    { _rdsShapes :: ![RenderShape]
    }
makeLenses ''RenderSystem
defineSystemKey ''RenderSystem

numInstances = 10000

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

    shapesWithBuffers <- forM shapes $ \(shapeType, shape) -> do
        withShape shape $ do
            modelM44sVector <- liftIO $ VM.replicate numInstances (identity :: M44 GLfloat)
            colorsVector    <- liftIO $ VM.replicate numInstances (0        :: V4 GLfloat)

            modelM44sBuffer    <- bufferDataV GL_DYNAMIC_DRAW modelM44sVector
            colorsBuffer       <- bufferDataV GL_DYNAMIC_DRAW colorsVector
            shader <- asks sProgram
            withArrayBuffer modelM44sBuffer $ 
                assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT
            withArrayBuffer colorsBuffer $ 
                assignFloatAttributeInstanced shader "aInstanceColor" GL_FLOAT 4

            return RenderShape 
                { rshShapeType                 = shapeType
                , rshShape                     = shape
                , rshInstanceColorsMVector     = colorsVector
                , rshInstanceColorsBuffer      = colorsBuffer
                , rshInstanceModelM44sMVector  = modelM44sVector
                , rshInstanceModelM44sBuffer   = modelM44sBuffer
                }

    registerSystem sysRender (RenderSystem shapesWithBuffers)


tickRenderSystem :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m ()
tickRenderSystem headM44 = do
    vrPal  <- viewSystem sysControls ctsVRPal
    player <- viewSystem sysControls ctsPlayer

    finalMatricesByEntityID <- profileMS' "getFinalMatrices" 2 $ getFinalMatrices

    -- Pulse the currently selected entity in blue
    selectedEntityID <- getSelectedEntityID
    let getEntityColorOrSelectedColor entityID = do
            if Just entityID == selectedEntityID
                then do
                    now <- (+0.2) . (*0.1) . (+1) . sin . (*6) <$> getNow
                    return $ hslColor 0.6 0.9 now
                else getEntityColor entityID
    -- Batch by entities sharing the same shape type

    shapes      <- viewSystem sysRender rdsShapes
    shapeCounts <- forM shapes $ \RenderShape{..} -> withShape rshShape $ do
        entityIDsForShape <- profileMS' "getEntityIDsForShapeType" 3 $ getEntityIDsForShapeType rshShapeType
        count <- profileMS' "writeVectors" 3 $ foldM (\i entityID -> do
            color <- getEntityColorOrSelectedColor entityID
            let modelM44 = fromMaybe identity $ Map.lookup entityID finalMatricesByEntityID
            liftIO $ do
                VM.write rshInstanceColorsMVector    i color
                VM.write rshInstanceModelM44sMVector i modelM44
            return (i+1)
            ) 0 entityIDsForShape 
        profileMS' "bufCols" 3 $ bufferSubDataV rshInstanceColorsBuffer    rshInstanceColorsMVector
        profileMS' "bufM44s" 3 $ bufferSubDataV rshInstanceModelM44sBuffer rshInstanceModelM44sMVector

        return (rshShape, count)

    -- Render the scene
    renderWith vrPal player headM44
        (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
        (\projM44 viewM44 -> do
            let projViewM44 = projM44 !*! viewM44
            profileMS' "entities" 2     $ renderEntities     projViewM44 shapeCounts
            profileMS' "entitiesText" 2 $ renderEntitiesText projViewM44 finalMatricesByEntityID
            )


renderEntities :: (MonadIO m, MonadState ECS m) 
               => M44 GLfloat -> [(Shape Uniforms, Int)] -> m ()
renderEntities projViewM44 shapes = do
    
    headM44 <- getHeadPose

    forM_ shapes $ \(shape, shapeCount) -> withShape shape $ do

        Uniforms{..} <- asks sUniforms
        uniformV3  uCamera (headM44 ^. translation)
        uniformM44 uProjectionView projViewM44

        profileMS' "Draw" 3 $ drawShapeInstanced (fromIntegral shapeCount)

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

            entityMatrixLocalNoScale <- getEntityPose entityID

            size <- getEntitySize entityID
            let entityMatrixLocal = entityMatrixLocalNoScale !*! scaleMatrix size

            
            -- See if we want to inherit our parent's matrix
            inherit <- getEntityInheritParentTransform entityID
            let entityMatrix = case (inherit, mParentMatrix) of
                    (InheritFull, Just (parentMatrix, _))        -> parentMatrix        !*! entityMatrixLocal
                    (InheritPose, Just (_, parentMatrixNoScale)) -> parentMatrixNoScale !*! entityMatrixLocal
                    _                                            ->                         entityMatrixLocal
                entityMatrixNoScale = case (inherit, mParentMatrix) of
                    (InheritFull, Just (parentMatrix, _))        -> parentMatrix        !*! entityMatrixLocalNoScale
                    (InheritPose, Just (_, parentMatrixNoScale)) -> parentMatrixNoScale !*! entityMatrixLocalNoScale
                    _                                            ->                         entityMatrixLocalNoScale

            -- Pass the calculated matrix down to each child so it can calculate its own final matrix
            children <- getEntityChildren entityID
            foldM (go (Just (entityMatrix, entityMatrixNoScale))) (Map.insert entityID entityMatrix accum) children
    foldM (go Nothing) mempty rootIDs

{-
getScaledMatrix :: MonadState ECS m => EntityID -> m (M44 GLfloat)
getScaledMatrix entityID = do
    pose <- getEntityPose entityID
    size <- getEntitySize entityID
    return $! pose !*! scaleMatrix size
-}
getEntityIDsForShapeType :: MonadState ECS m => ShapeType -> m [EntityID]
getEntityIDsForShapeType shapeType = Map.keys . Map.filter (== shapeType) <$> getComponentMap cmpShapeType


renderEntitiesText :: (MonadState ECS m, MonadIO m) 
                   => M44 GLfloat -> Map EntityID (M44 GLfloat) -> m ()
renderEntitiesText projViewM44 finalMatricesByEntityID = do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    entitiesWithText <- Map.toList <$> getComponentMap cmpTextRenderer
    forM_ entitiesWithText $ \(entityID, textRenderer) -> do
        color <- getEntityTextColor entityID 
        
        textM44 <- getEntityTextPose entityID
        let entityM44 = fromMaybe identity $ Map.lookup entityID finalMatricesByEntityID
            finalM44 = entityM44 !*! textM44

        renderText textRenderer (projViewM44 !*! finalM44) (color ^. _xyz)

    entitiesWithOnStart <- Map.toList <$> getComponentMap cmpOnStartExpr
    forM_ entitiesWithOnStart $ \(entityID, codeExprKey) -> 
        traverseM_ (viewSystem sysCodeEditor (cesCodeEditors . at codeExprKey)) $ \editor -> do
            parentPose <- getEntityPose entityID
            V3 _ _ sizeZ <- getEntitySize entityID

            --let codeModelM44 = parentPose !*! (identity & translation .~ V3 (-0.04) (0.04) (0.151)) !*! scaleMatrix 0.2
            let codeModelM44 = parentPose !*! translateMatrix (V3 0 0 sizeZ) !*! scaleMatrix 0.01 

            -- Render code in white
            renderText (editor ^. cedCodeRenderer) (projViewM44 !*! codeModelM44) (V3 1 1 1)

            let errorsModelM44 = codeModelM44 !*! translateMatrix (V3 1 0 0)

            -- Render errors in light red
            renderText (editor ^. cedErrorRenderer) (projViewM44 !*! errorsModelM44) (V3 1 0.5 0.5)

    glDisable GL_BLEND

