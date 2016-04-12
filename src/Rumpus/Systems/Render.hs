{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

-- Restricting exports to help GHC optimizer
module Rumpus.Systems.Render 
    ( initRenderSystem
    , tickRenderSystem
    ) where
import PreludeExtra

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Rumpus.Systems.Shared
import Rumpus.Systems.Selection
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Rumpus.Systems.Text
import Graphics.GL.TextBuffer

import qualified Data.Vector.Unboxed as V
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Exception

data Uniforms = Uniforms
    { uProjectionView :: UniformLocation (M44 GLfloat)
    , uCamera         :: UniformLocation (V3  GLfloat)
    } deriving (Data)

data RenderShape = RenderShape
    { rshShapeType                 :: ShapeType
    , rshShape                     :: Shape Uniforms
    , rshStreamingArrayBuffer      :: StreamingArrayBuffer
    , rshInstanceColorsBuffer      :: ArrayBuffer (V4 GLfloat)
    , rshInstanceModelM44sBuffer   :: ArrayBuffer (M44 GLfloat)
    , rshResetShapeInstanceBuffers :: IO ()
    }

data RenderSystem = RenderSystem 
    { _rdsShapes :: ![RenderShape]
    }
makeLenses ''RenderSystem
defineSystemKey ''RenderSystem

maxInstances :: Int
maxInstances = 2048

initRenderSystem :: (MonadIO m, MonadState ECS m) => m ()
initRenderSystem = do
    glEnable GL_DEPTH_TEST
    glClearColor 0 0 0.1 1

    basicProg   <- createShaderProgram "resources/shaders/default.vert" "resources/shaders/default.frag"

    --planeGeo    <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 1
    cubeGeo     <- cubeGeometry (V3 1 1 1) 1
    sphereGeo   <- icosahedronGeometry 1 3 -- radius subdivisions
    
    --planeShape  <- makeShape planeGeo  basicProg
    cubeShape   <- makeShape cubeGeo   basicProg
    sphereShape <- makeShape sphereGeo basicProg

    --let shapes = [(CubeShape, cubeShape), (SphereShape, sphereShape), (StaticPlaneShape, planeShape)]
    let shapes = [(CubeShape, cubeShape), (SphereShape, sphereShape)]

    shapesWithBuffers <- forM shapes $ \(shapeType, shape) -> do
        withShape shape $ do
            let streamingBufferCapacity = maxInstances * 64
            sab              <- makeSAB streamingBufferCapacity
            modelM44sBuffer  <- bufferDataEmpty GL_STREAM_DRAW streamingBufferCapacity (Proxy :: Proxy (M44 GLfloat))
            colorsBuffer     <- bufferDataEmpty GL_STREAM_DRAW streamingBufferCapacity (Proxy :: Proxy (V4  GLfloat))
            
            --let resetShapeInstanceBuffers = profileMS "reset" 0 $ withShape shape $ do
            let resetShapeInstanceBuffers = withShape shape $ do
                    shader <- asks sProgram
                    withArrayBuffer modelM44sBuffer $ do
                        resetSABBuffer sab modelM44sBuffer
                        assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT

                    withArrayBuffer colorsBuffer $ do
                        resetSABBuffer sab colorsBuffer
                        assignFloatAttributeInstanced  shader "aInstanceColor" GL_FLOAT 4
            liftIO resetShapeInstanceBuffers

            return RenderShape 
                { rshShapeType                 = shapeType
                , rshShape                     = shape
                , rshStreamingArrayBuffer      = sab
                , rshInstanceColorsBuffer      = colorsBuffer
                , rshInstanceModelM44sBuffer   = modelM44sBuffer
                , rshResetShapeInstanceBuffers = resetShapeInstanceBuffers
                }

    registerSystem sysRender (RenderSystem shapesWithBuffers)


tickRenderSystem :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m ()
tickRenderSystem headM44 = do
    
    finalMatricesByEntityID <- getFinalMatrices
    colorsMap               <- getComponentMap myColor

    -- Pulse the currently selected entity in blue
    selectedEntityID <- getSelectedEntityID
    now <- (+0.2) . (*0.1) . (+1) . sin . (*6) <$> getNow
    let colorForEntity entityID 
            | Just entityID == selectedEntityID = hslColor 0.6 0.9 now
            | otherwise = fromMaybe 1 $ Map.lookup entityID colorsMap

    -- Batch by entities sharing the same shape type

    shapes      <- viewSystem sysRender rdsShapes
    shapeCounts <- forM shapes $ \RenderShape{..} -> withShape rshShape $ do
        --let shapeName = show rshShapeType ++ " "
        entityIDsForShape <- getEntityIDsForShapeType rshShapeType
        let count = V.length entityIDsForShape
        writeSAB rshStreamingArrayBuffer (fromIntegral count) rshResetShapeInstanceBuffers $ do
            fillSABBuffer rshInstanceColorsBuffer $ \i -> do
                let entityID = entityIDsForShape V.! i
                    color = colorForEntity entityID
                return color
            fillSABBuffer rshInstanceModelM44sBuffer $ \i -> do
                let entityID = entityIDsForShape V.! i
                    modelM44 = fromMaybe identity $ Map.lookup entityID finalMatricesByEntityID
                return modelM44

        return (rshShape, rshStreamingArrayBuffer, count)

    -- Render the scene
    vrPal  <- viewSystem sysControls ctsVRPal
    renderWith vrPal headM44 $ \projM44 viewM44 -> do
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        let projViewM44 = projM44 !*! viewM44
        renderEntities     projViewM44 shapeCounts
        renderEntitiesText projViewM44 finalMatricesByEntityID


renderEntities :: (MonadIO m, MonadState ECS m) 
               => M44 GLfloat -> [(Shape Uniforms, StreamingArrayBuffer, Int)] -> m ()
renderEntities projViewM44 shapes = do
    
    headM44 <- getHeadPose

    forM_ shapes $ \(shape, sab, shapeCount) -> withShape shape $ do

        Uniforms{..} <- asks sUniforms
        uniformV3  uCamera (headM44 ^. translation)
        uniformM44 uProjectionView projViewM44

        drawSAB sab (fromIntegral shapeCount)


-- Perform a breadth-first traversal of entities with no parents, 
-- accumulating their matrix mults all the way down into any children.
-- This avoids duplicate matrix multiplications.
getFinalMatrices :: (MonadIO m, MonadState ECS m) => m (Map EntityID (M44 GLfloat))
getFinalMatrices = do
    poseMap                   <- getComponentMap myPose
    poseScaledMap             <- getComponentMap myPoseScaled
    childrenMap               <- getComponentMap myChildren
    parentMap                 <- getComponentMap myParent
    sizeMap                   <- getComponentMap mySize
    inheritParentTransformMap <- getComponentMap myInheritParentTransform
    
    let entityIDs           = Set.fromList . Map.keys $ poseMap
        entityIDsWithChild  = Set.fromList . Map.keys $ childrenMap
        entityIDsWithParent = Set.fromList . Map.keys $ parentMap
        rootIDs = Set.union entityIDs entityIDsWithChild Set.\\ entityIDsWithParent
        go mParentMatrix !accum entityID = 

            let inherit                  = Map.lookup entityID inheritParentTransformMap
                entityMatrixLocalNoScale = Map.lookupDefault identity entityID poseMap
                entityMatrixLocal        = Map.lookupDefault identity entityID poseScaledMap

                parentTransform          = case (inherit, mParentMatrix) of
                    (Just InheritFull, Just (parentMatrix, _))        -> (parentMatrix        !*!) 
                    (Just InheritPose, Just (_, parentMatrixNoScale)) -> (parentMatrixNoScale !*!) 
                    _                                                 -> id

                entityMatrix        = parentTransform entityMatrixLocal
                entityMatrixNoScale = parentTransform entityMatrixLocalNoScale

                children            = Map.lookupDefault [] entityID childrenMap
            -- Pass the calculated matrix down to each child so it can calculate its own final matrix
            in foldl' (go (Just (entityMatrix, entityMatrixNoScale))) (Map.insert entityID entityMatrix accum) children
        calcMatricesForRootIDs = foldl' (go Nothing) mempty

        !finalMatricesByEntityID = calcMatricesForRootIDs rootIDs

        -- !finalMatricesByEntityID = parMapChunks 512 calcMatricesForRootIDs rootIDs
    --finalMatricesByEntityID <- naiveParMapChunks 512 calcMatricesForRootIDs rootIDs

    return finalMatricesByEntityID



getEntityIDsForShapeType :: MonadState ECS m => ShapeType -> m (V.Vector EntityID)
getEntityIDsForShapeType shapeType = V.fromList . Map.keys . Map.filter (== shapeType) <$> getComponentMap myShapeType


renderEntitiesText :: (MonadState ECS m, MonadIO m) 
                   => M44 GLfloat -> Map EntityID (M44 GLfloat) -> m ()
renderEntitiesText projViewM44 finalMatricesByEntityID = do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    font <- getFont
    withSharedFont font projViewM44 $ do

        entitiesWithText <- Map.toList <$> getComponentMap myTextRenderer
        forM_ entitiesWithText $ \(entityID, textRenderer) -> do
            --color <- getEntityTextColor entityID
            
            let parentM44 = Map.lookupDefault identity entityID finalMatricesByEntityID
            textM44 <- getEntityTextCachedM44 entityID

            renderTextPreCorrectedOfSameFont textRenderer (parentM44 !*! textM44)

        entitiesWithStart <- Map.toList <$> getComponentMap myStartExpr
        forM_ entitiesWithStart $ \(entityID, codeExprKey) -> 
            traverseM_ (viewSystem sysCodeEditor (cesCodeEditors . at codeExprKey)) $ \editor -> do
                parentPose   <- getEntityPose entityID
                V3 _ _ sizeZ <- getEntitySize entityID

                let codeModelM44 = parentPose !*! translateMatrix (V3 0 0 (sizeZ/2 + 0.01)) !*! scaleMatrix 0.008

                -- Render code in white
                renderTextPreCorrectedOfSameFont (editor ^. cedCodeRenderer) (codeModelM44 !*! editor ^. cedCodeRenderer . txrCorrectionM44)

                let errorsModelM44 = codeModelM44 !*! translateMatrix (V3 50 0 0)

                -- Render errors in light red
                renderTextPreCorrectedOfSameFont (editor ^. cedErrorRenderer) (errorsModelM44 !*! editor ^. cedErrorRenderer . txrCorrectionM44)

    glDisable GL_BLEND




--- Parallel matrix evaluation experiments

-- | Takes a [input] and a function from [input] to some monoid b, 
-- runs the function on chunks of [input], and glues the results back together.
parMapChunks :: (Foldable f, Monoid b, NFData b) => Int -> ([a] -> b) -> f a -> b
parMapChunks n f xs = mconcat $ parMap rdeepseq f (chunkInto n (toList xs))

-- | Naive reimplementation of parMapChunks for comparing.
-- Only spins up threads if length xs > n
naiveParMapChunks :: (Foldable f, MonadIO m, Monoid b, NFData b) => Int -> ([a] -> b) -> f a -> m b
naiveParMapChunks n f xs = do
    -- Split the input into chunks
    let chunks = chunkInto n (toList xs)
        (localChunk:threadChunks) = if null chunks then [[]] else chunks

    -- Spin up threads for chunks 1..n
    threads <- forM threadChunks (\xsForThread -> liftIO $ do
        resultVar <- newEmptyMVar
        _ <- forkIO $ do
            let !result = force (f xsForThread)  
            putMVar resultVar result
        return resultVar)

    -- Calculate chunk 0 on this thread while the other threads run
    localResults <- liftIO $ evaluate (force f localChunk) 

    -- Get thread results
    threadResults <- mapM (liftIO . takeMVar) threads

    return . mconcat $ localResults:threadResults  

chunkInto :: Int -> [t] -> [[t]]
chunkInto n l = go l
    where 
        go [] = []
        go xs = let (x,xs') = splitAt n xs
                in x:go xs' 