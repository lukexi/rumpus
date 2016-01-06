{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Graphics.GL.Pal
import Graphics.VR.Pal

import Control.Monad
import Control.Monad.State
import Control.Lens.Extra
import Data.Maybe
import qualified Data.Map as Map

import Physics.Bullet
import Sound.Pd

import Render
import Entity
import Types
import Control
import Control.Monad.Reader
import Data.Yaml hiding ((.=))
import Data.Foldable

import qualified Spatula

traverseM :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
traverseM f x = f >>= traverse x
traverseM_ :: (Monad m, Foldable t) => m (t a) -> (a -> m b) -> m ()
traverseM_ f x = f >>= traverse_ x

createRenderSystem :: IO [(ShapeType, Shape Uniforms)]
createRenderSystem = do
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
    return shapes

createPhysicsSystem :: IO DynamicsWorld
createPhysicsSystem = createDynamicsWorld mempty

main :: IO ()
main = withPd $ \pd -> do
    mapM_ (addToLibPdSearchPath pd)
        ["resources/pd-kit", "resources/pd-kit/list-abs"]

    vrPal  <- initVRPal "Rumpus" [UseOpenVR]

    shapes <- createRenderSystem

    dynamicsWorld <- createPhysicsSystem

    let worldStatic = WorldStatic
            { _wlsDynamicsWorld = dynamicsWorld
            , _wlsShapes        = shapes
            , _wlsVRPal         = vrPal
            , _wlsPd            = pd
            }
        world = newWorld & wldOpenALSourcePool .~ zip [1..] (pdSources pd)
                         & wldPlayer .~ if gpRoomScale vrPal == RoomScale 
                                        then newPose
                                        else newPose & posPosition .~ V3 0 1 5

    encodeFile "testScene.yaml" [newEntity]
    entities <- decodeFileEither "testScene.yaml"
    print (entities :: Either ParseException [Entity])

    void . flip runReaderT worldStatic . flip runStateT world $ do 

        mapM_ createEntity =<< Spatula.initScene

        whileVR vrPal $ \headM44 hands -> do
            
            -- Collect control events into the events channel to be read by entities during update
            controlEventsSystem vrPal headM44 hands

            attachmentsSystem

            isPlaying <- use wldPlaying
            if isPlaying  
                then do
                    scriptingSystem
                    
                    physicsSystem
                    
                    syncPhysicsPosesSystem

                    collisionsSystem

                    editingSystem
                else do
                    return ()

            openALSystem headM44

            renderSystem headM44

editingSystem :: WorldMonad ()
editingSystem = do
    let f handName event = do
            mHandEntityID <- listToMaybe <$> getEntityIDsWithName handName
            forM_ mHandEntityID $ \handEntityID -> case event of
                HandStateEvent hand -> 
                    setEntityPose handEntityID (poseFromMatrix (hand ^. hndMatrix))
                HandButtonEvent HandButtonTrigger ButtonDown -> do
                    putStrLnIO "ButtonDown!"
                    overlappingEntityIDs <- getEntityGhostOverlappingEntityIDs handEntityID
                    forM_ overlappingEntityIDs $ \touchedID -> do
                        name <- getEntityName touchedID
                        when (name /= "Floor") $ 
                            attachEntity handEntityID touchedID
                HandButtonEvent HandButtonTrigger ButtonUp -> do
                    -- Copy the current pose to the scene file and save it
                    mAttachedEntityID <- use (wldComponents . cmpAttachment . at handEntityID)
                    forM_ mAttachedEntityID $ \(Attachment attachedEntityID _offset) -> do
                        wldComponents . cmpAttachment . at handEntityID .= Nothing
                        withEntityRigidBody attachedEntityID $ \rigidBody ->
                            setRigidBodyKinematic rigidBody False

                        currentPose <- fromMaybe newPose <$> use (wldComponents . cmpPose . at attachedEntityID)
                        wldScene . at attachedEntityID . traverse . entPose .= currentPose
                        scene <- use wldScene
                        liftIO $ encodeFile "spatula/spatula.yaml" (Map.toList scene)
                _ -> return ()

    withLeftHandEvents (f "Left Hand")
    withRightHandEvents (f "Right Hand")

getEntityIDsWithName :: MonadState World m => String -> m [EntityID]
getEntityIDsWithName name = 
    Map.keys . Map.filter (== name) <$> use (wldComponents . cmpName)

getEntityGhostOverlappingEntityIDs :: (MonadState World m, MonadIO m) => EntityID -> m [EntityID]
getEntityGhostOverlappingEntityIDs entityID = do
    overlappingCollisionObjects <- getEntityGhostOverlapping entityID
    map unCollisionObjectID <$> mapM getCollisionObjectID overlappingCollisionObjects

getEntityName :: MonadState World m => EntityID -> m String
getEntityName entityID = fromMaybe "No Name" <$> use (wldComponents . cmpName . at entityID)

attachEntity :: (MonadIO m, MonadState World m) => EntityID -> EntityID -> m ()
attachEntity entityID toEntityID = do

    putStrLnIO ("Attaching " ++ show entityID ++ " to " ++ show toEntityID)
    entityPose   <- fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)
    toEntityPose <- fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)
    let offset = subtractPoses toEntityPose entityPose
    wldComponents . cmpAttachment . at entityID ?= Attachment toEntityID offset
    withEntityRigidBody toEntityID $ \rigidBody ->
        setRigidBodyKinematic rigidBody True

attachmentsSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => m ()
attachmentsSystem = do
    attachments <- Map.toList <$> use (wldComponents . cmpAttachment)
    forM_ attachments $ \(entityID, Attachment toEntityID offset) -> do
        pose <- fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)
        putStrLnIO ("Moving " ++ show toEntityID ++ " to " ++ show pose)
        setEntityPose toEntityID (addPoses pose offset)
        -- wldComponents . cmpPose . at toEntityID ?= addPoses pose offset

scriptingSystem :: WorldMonad ()
scriptingSystem = do
    -- Process the update functions of each entity
    mapM_ (\(entityID, update) -> update entityID) 
        =<< Map.toList <$> use (wldComponents . cmpUpdate) 

renderSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => M44 GLfloat -> m ()
renderSystem headM44 = do
    vrPal <- view wlsVRPal
    -- Render the scene
    player <- use wldPlayer
    renderWith vrPal player headM44
        (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
        renderSimulation

physicsSystem :: (MonadIO m, MonadReader WorldStatic m) => m ()
physicsSystem = do
    dynamicsWorld <- view wlsDynamicsWorld
    stepSimulation dynamicsWorld 90

syncPhysicsPosesSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => m ()
syncPhysicsPosesSystem = do
    -- Sync rigid bodies with entity poses
    traverseM_ (Map.toList <$> use (wldComponents . cmpRigidBody)) $ 
        \(entityID, rigidBody) -> do
            pose <- uncurry Pose <$> getBodyState rigidBody
            wldComponents . cmpPose . at entityID ?= pose



collisionsSystem :: WorldMonad ()
collisionsSystem = do
    dynamicsWorld <- view wlsDynamicsWorld
    -- Tell objects about any collisions
    collisions <- getCollisions dynamicsWorld
    
    forM_ collisions $ \collision -> do
        let bodyAID = (fromIntegral . unCollisionObjectID . cbBodyAID) collision
            bodyBID = (fromIntegral . unCollisionObjectID . cbBodyBID) collision
            appliedImpulse = cbAppliedImpulse collision
        mapM_ (\onCollision -> onCollision bodyAID bodyBID appliedImpulse)
            =<< use (wldComponents . cmpCollision . at bodyAID)
        mapM_ (\onCollision -> onCollision bodyBID bodyAID appliedImpulse)
            =<< use (wldComponents . cmpCollision . at bodyBID)

openALSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => M44 GLfloat -> m ()
openALSystem headM44 = do
    -- Update souce and listener poitions
    alListenerPose (poseFromMatrix headM44)
    mapM_ (\(entityID, sourceID) -> do
        position <- view posPosition . fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)
        alSourcePosition sourceID position)
        =<< Map.toList <$> use (wldComponents . cmpSoundSource)
