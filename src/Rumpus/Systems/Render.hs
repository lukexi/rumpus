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
import Rumpus.Types
import Graphics.GL.TextBuffer

import qualified Data.Vector.Storable.Mutable as VM
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Exception

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

maxInstances :: Int
maxInstances = 10000

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
            modelM44sVector <- liftIO $ VM.replicate maxInstances (identity :: M44 GLfloat)
            colorsVector    <- liftIO $ VM.replicate maxInstances (0        :: V4 GLfloat)

            modelM44sBuffer <- bufferDataV GL_DYNAMIC_DRAW modelM44sVector
            colorsBuffer    <- bufferDataV GL_DYNAMIC_DRAW colorsVector
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
        let shapeName = show rshShapeType ++ " "
        entityIDsForShape <- profileMS' (shapeName ++ "getEntityIDsForShapeType") 3 $ getEntityIDsForShapeType rshShapeType
        count <- profileMS' (shapeName ++ "writeVectors") 3 $ foldM (\i entityID -> do
            color <- getEntityColorOrSelectedColor entityID
            let modelM44 = fromMaybe identity $ Map.lookup entityID finalMatricesByEntityID
            liftIO $ do
                VM.write rshInstanceColorsMVector    i color
                VM.write rshInstanceModelM44sMVector i modelM44
            return (i+1)
            ) 0 entityIDsForShape
        let countStr = "("++show count++") "
        profileMS' (shapeName ++ countStr ++ "bufCols") 3 $ bufferSubDataV rshInstanceColorsBuffer    rshInstanceColorsMVector
        profileMS' (shapeName ++ countStr ++ "bufM44s") 3 $ bufferSubDataV rshInstanceModelM44sBuffer rshInstanceModelM44sMVector
        --printIO (rshShapeType, count)
        return (rshShape, count)

    -- Render the scene
    renderWith vrPal player headM44
        (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
        (\projM44 viewM44 -> do
            let projViewM44 = projM44 !*! viewM44
            profileMS' "renderEntities" 2     $ renderEntities     projViewM44 shapeCounts
            profileMS' "renderEntitiesText" 2 $ renderEntitiesText projViewM44 finalMatricesByEntityID
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
getFinalMatrices :: (MonadIO m, MonadState ECS m) => m (Map EntityID (M44 GLfloat))
getFinalMatrices = do
    poseMap                   <- getComponentMap cmpPose
    childrenMap               <- getComponentMap cmpChildren
    parentMap                 <- getComponentMap cmpParent
    sizeMap                   <- getComponentMap cmpSize
    inheritParentTransformMap <- getComponentMap cmpInheritParentTransform
    
    let entityIDs           = Set.fromList . Map.keys $ poseMap
        entityIDsWithChild  = Set.fromList . Map.keys $ childrenMap
        entityIDsWithParent = Set.fromList . Map.keys $ parentMap
        rootIDs = Set.union entityIDs entityIDsWithChild Set.\\ entityIDsWithParent
        go mParentMatrix !accum entityID = 

            let size                     = Map.lookupDefault 1 entityID sizeMap
                inherit                  = Map.lookup entityID inheritParentTransformMap
                entityMatrixLocalNoScale = Map.lookupDefault identity entityID poseMap
                entityMatrixLocal        = entityMatrixLocalNoScale !*! scaleMatrix size

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

