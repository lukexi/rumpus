{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
module Rumpus.Types where
import PreludeExtra

import GHC.Word
import GHC.Generics

import Data.Yaml
import Data.Aeson.Types

import TinyRick
import TinyRick.Recompiler2


type EntityID = Word32

type EntityMap a = Map EntityID a

data ShapeType = NoShape | CubeShape | SphereShape | StaticPlaneShape deriving (Eq, Show, Ord, Enum, Generic, FromJSON)

data PhysicsProperties = IsKinematic | NoContactResponse deriving (Eq, Show, Generic, FromJSON)

type WorldMonad = StateT World (ReaderT WorldStatic IO)

data WorldEvent = GLFWEvent Event
                | VREvent VREvent 
                deriving Show

data Persistence = Transient | Persistent deriving (Eq, Show, Generic, FromJSON)

type OnUpdate = EntityID -> WorldMonad ()
nullOnUpdate :: OnUpdate
nullOnUpdate _entityID = return ()

type OnStart = EntityID -> WorldMonad (Maybe Dynamic)
nullOnStart :: OnStart
nullOnStart _entityID = return Nothing

type CollidedWithID = EntityID
type CollisionImpulse = GLfloat
type OnCollision = EntityID -> CollidedWithID -> CollisionImpulse -> WorldMonad ()
nullOnCollision :: OnCollision
nullOnCollision _entityID _collidedWithID _collisionImpulse = return ()

data WorldStatic = WorldStatic
    { _wlsVRPal         :: !VRPal
    , _wlsDynamicsWorld :: !DynamicsWorld
    , _wlsPd            :: !PureData
    , _wlsShapes        :: ![(ShapeType, Shape Uniforms)]
    , _wlsFont          :: !Font
    , _wlsGHCChan       :: !(TChan CompilationRequest)
    }

data Scene = Scene
    { _scnName     :: String
    , _scnEntities :: !(Map EntityID Entity)
    }

newScene :: Scene
newScene = Scene { _scnName = "NewScene", _scnEntities = mempty }

data World = World
    { _wldPlayer           :: !(Pose GLfloat)
    , _wldPlayerHeadM44    :: !(M44 GLfloat)
    , _wldComponents       :: !Components
    , _wldEvents           :: ![WorldEvent]
    , _wldOpenALSourcePool :: ![(Int, OpenALSource)]
    , _wldPlaying          :: !Bool
    , _wldEntityLibrary    :: !(Map String Entity)
    , _wldScene            :: !Scene
    , _wldSelectedEntityID :: !(Maybe EntityID)
    }


newWorld :: World
newWorld = World
    { _wldPlayer = Pose (V3 0 0 0) (axisAngle (V3 0 1 0) 0)
    , _wldPlayerHeadM44 = identity
    , _wldComponents = newComponents
    , _wldEvents = []
    , _wldOpenALSourcePool = []
    , _wldPlaying = False
    , _wldEntityLibrary = mempty
    , _wldScene = newScene
    , _wldSelectedEntityID = Nothing
    }

data Attachment = Attachment EntityID (Pose GLfloat)

data Lifetime = Lifetime UTCTime NominalDiffTime

data Components = Components
    { _cmpName              :: EntityMap String
    , _cmpPose              :: EntityMap (Pose GLfloat)
    , _cmpSize              :: EntityMap (V3 GLfloat)
    , _cmpShape             :: EntityMap ShapeType
    , _cmpScale             :: EntityMap (V3 GLfloat)
    , _cmpColor             :: EntityMap (V4 GLfloat)
    , _cmpOnStart           :: EntityMap OnStart
    , _cmpOnUpdate          :: EntityMap OnUpdate
    , _cmpOnCollision       :: EntityMap OnCollision
    , _cmpOnStartEditor     :: EntityMap CodeEditor
    , _cmpOnUpdateEditor    :: EntityMap CodeEditor
    , _cmpOnCollisionEditor :: EntityMap CodeEditor
    , _cmpScriptData        :: EntityMap Dynamic
    , _cmpParent            :: EntityMap EntityID
    , _cmpRigidBody         :: EntityMap RigidBody
    , _cmpSpring            :: EntityMap SpringConstraint
    , _cmpPhysicsProperties :: EntityMap [PhysicsProperties]
    , _cmpPdPatch           :: EntityMap Patch
    , _cmpSoundSource       :: EntityMap OpenALSource
    , _cmpAttachment        :: EntityMap Attachment
    , _cmpLifetime          :: EntityMap Lifetime
    }

-- not yet used
data PhysicsComponents = PhysicsComponents
    { _pcRigidBody   :: EntityMap RigidBody
    , _pcSpring      :: EntityMap SpringConstraint
    , _pcCollision   :: EntityMap OnCollision
    }

newComponents :: Components
newComponents = Components
    { _cmpName              = mempty
    , _cmpPose              = mempty
    , _cmpSize              = mempty
    , _cmpShape             = mempty
    , _cmpScale             = mempty
    , _cmpColor             = mempty
    , _cmpOnStart           = mempty
    , _cmpOnUpdate          = mempty
    , _cmpOnCollision       = mempty
    , _cmpOnStartEditor     = mempty
    , _cmpOnUpdateEditor    = mempty
    , _cmpOnCollisionEditor = mempty
    , _cmpScriptData        = mempty
    , _cmpParent            = mempty
    , _cmpRigidBody         = mempty
    , _cmpSpring            = mempty
    , _cmpPhysicsProperties = mempty
    , _cmpPdPatch           = mempty
    , _cmpSoundSource       = mempty
    , _cmpAttachment        = mempty
    , _cmpLifetime          = mempty
    }

data Entity = Entity
    { _entName        :: !String
    , _entSize        :: !(V3 GLfloat)
    , _entShape       :: !(ShapeType)
    , _entPose        :: !(Pose GLfloat)
    , _entScale       :: !(V3 GLfloat)
    , _entColor       :: !(V4 GLfloat)
    , _entPhysProps   :: ![PhysicsProperties]
    , _entChildren    :: ![Entity]
    , _entMass        :: !Float
    , _entLifetime    :: !(Maybe Float)
    , _entPdPatch     :: !(Maybe FilePath)
    , _entOnStart     :: !(Maybe FilePath)
    , _entOnUpdate    :: !(Maybe FilePath)
    , _entOnCollision :: !(Maybe FilePath)
    } deriving (Show, Generic)

newEntity :: Entity
newEntity = Entity
    { _entName        = "NewEntity"
    , _entSize        = V3 1 1 1
    , _entShape       = NoShape
    , _entPose        = newPose
    , _entScale       = V3 1 1 1
    , _entColor       = V4 1 1 1 1
    , _entPhysProps   = []
    , _entChildren    = []
    , _entMass        = 1
    , _entLifetime    = Nothing
    , _entPdPatch     = Nothing
    , _entOnStart     = Nothing
    , _entOnUpdate    = Nothing
    , _entOnCollision = Nothing
    }


data CodeEditor = CodeEditor
    { _cedResultTChan   :: TChan CompilationResult
    , _cedCodeRenderer  :: TextRenderer
    , _cedErrorRenderer :: TextRenderer
    }

data Uniforms = Uniforms
    { uModelViewProjection :: UniformLocation (M44 GLfloat)
    , uInverseModel        :: UniformLocation (M44 GLfloat)
    , uModel               :: UniformLocation (M44 GLfloat)
    , uCamera              :: UniformLocation (V3  GLfloat)
    , uDiffuse             :: UniformLocation (V4  GLfloat)
    } deriving (Data)


-----------------
-- JSON instances
-----------------
-- NOTE: we're using 
-- toJSON = genericToJSON defaultOptions
-- to work around "No explicit implementation" bug in the current
-- version of Aeson caused by an errant minimal definition pragma
instance FromJSON a => FromJSON (V4 a)
instance FromJSON a => FromJSON (V3 a)
instance FromJSON a => FromJSON (V2 a)
instance FromJSON a => FromJSON (Quaternion a)
instance ToJSON a => ToJSON (V4 a) where
    toJSON = genericToJSON defaultOptions
instance ToJSON a => ToJSON (V3 a) where
    toJSON = genericToJSON defaultOptions
instance ToJSON a => ToJSON (V2 a) where
    toJSON = genericToJSON defaultOptions
instance ToJSON a => ToJSON (Quaternion a) where
    toJSON = genericToJSON defaultOptions

poseJSONOptions :: Options
poseJSONOptions = defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON a => FromJSON (Pose a) where
    parseJSON = genericParseJSON poseJSONOptions
instance ToJSON a => ToJSON (Pose a) where
    toJSON     = genericToJSON poseJSONOptions


entityJSONOptions :: Options
entityJSONOptions = defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON Entity where
    parseJSON = genericParseJSON entityJSONOptions
instance ToJSON Entity where
    toJSON     = genericToJSON entityJSONOptions

-- These can be put back in the deriving clause once the aforementioned Aeson bug is fixed
instance ToJSON ShapeType where
    toJSON = genericToJSON defaultOptions
instance ToJSON PhysicsProperties where
    toJSON = genericToJSON defaultOptions
instance ToJSON Persistence where
    toJSON = genericToJSON defaultOptions

makeLenses ''WorldStatic
makeLenses ''World
makeLenses ''Entity
makeLenses ''Components
makeLenses ''Scene
makeLenses ''CodeEditor

raycastCursorHits :: (MonadIO m, MonadState World m) 
                  => Window -> DynamicsWorld -> M44 GLfloat -> m ()
raycastCursorHits window dynamicsWorld projMat = do
    playerPose <- use wldPlayer
    cursorRay  <- cursorPosToWorldRay window projMat playerPose

    mRayResult <- rayTestClosest dynamicsWorld cursorRay
    forM_ mRayResult $ \rayResult -> do
        bodyID <- getCollisionObjectID (rrCollisionObject rayResult)
        -- Convert the hit location into model space
        -- (position, orientation) <- getBodyState (cube ^. cubBody)
        -- let model = mkTransformation orientation position
        --     pointOnModel = worldPointToModelPoint model (rrLocation rayResult)
        let _hitInWorld = rrLocation rayResult
            entityID = fromIntegral (unCollisionObjectID bodyID) :: EntityID
        return entityID
