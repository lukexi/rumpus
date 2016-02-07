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
import Rumpus.Orphans ()

import Data.Yaml
import Data.Aeson.Types

import TinyRick
import Halive.Recompiler


type EntityID = Word32

type EntityMap a = Map EntityID a

data ShapeType = NoShape | CubeShape | SphereShape | StaticPlaneShape 
    deriving (Eq, Show, Ord, Enum, Generic, FromJSON)

data PhysicsProperties = IsKinematic | NoContactResponse 
    deriving (Eq, Show, Generic, FromJSON)

type WorldMonad = StateT World (ReaderT WorldStatic IO)

data WorldEvent = GLFWEvent Event
                | VREvent VREvent 
                deriving Show

data Persistence = Transient | Persistent 
    deriving (Eq, Show, Generic, FromJSON)


-- | OnStart function
type OnStart = EntityID -> WorldMonad (Maybe Dynamic)

nullOnStart :: OnStart
nullOnStart _entityID = return Nothing


-- | OnUpdate function
type OnUpdate = EntityID -> WorldMonad ()

nullOnUpdate :: OnUpdate
nullOnUpdate _entityID = return ()

-- | OnDrag function
type OnDrag = EntityID -> V3 GLfloat -> WorldMonad ()
nullOnDrag :: OnDrag
nullOnDrag _entityID _dragDistance = return ()

-- | OnCollision functions
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
    { _wldPlayer             :: !(Pose GLfloat)
    , _wldPlayerHeadM44      :: !(M44 GLfloat)
    , _wldComponents         :: !Components
    , _wldEvents             :: ![WorldEvent]
    , _wldOpenALSourcePool   :: ![(Int, OpenALSource)]
    , _wldPlaying            :: !Bool
    , _wldEntityLibrary      :: !(Map String Entity)
    , _wldScene              :: !Scene
    , _wldSelectedEntityID   :: !(Maybe EntityID)
    , _wldCurrentEditorFrame :: !(Maybe EntityID)
    , _wldCodeEditors        :: !(Map CodeExpressionKey CodeEditor)
    }

type CodeExpressionKey = (FilePath, String)

newWorld :: World
newWorld = World
    { _wldPlayer             = newPose
    , _wldPlayerHeadM44      = identity
    , _wldComponents         = newComponents
    , _wldEvents             = []
    , _wldOpenALSourcePool   = []
    , _wldPlaying            = False
    , _wldEntityLibrary      = mempty
    , _wldScene              = newScene
    , _wldSelectedEntityID   = Nothing
    , _wldCurrentEditorFrame = Nothing
    , _wldCodeEditors        = mempty
    }

data Attachment = Attachment EntityID (Pose GLfloat)

data Lifetime = Lifetime UTCTime NominalDiffTime

data Constraint = RelativePositionTo EntityID (V3 GLfloat)

type HandEntityID = EntityID
-- data Drag = Drag HandEntityID (Pose GLfloat)
data Drag = Drag HandEntityID (V3 GLfloat)

data Components = Components
    { _cmpName              :: EntityMap String
    , _cmpPose              :: EntityMap (Pose GLfloat)
    , _cmpSize              :: EntityMap (V3 GLfloat)
    , _cmpShape             :: EntityMap ShapeType
    , _cmpColor             :: EntityMap (V4 GLfloat)
    , _cmpOnStart           :: EntityMap OnStart
    , _cmpOnUpdate          :: EntityMap OnUpdate
    , _cmpOnCollision       :: EntityMap OnCollision
    , _cmpOnStartExpr       :: EntityMap CodeExpressionKey
    , _cmpOnUpdateExpr      :: EntityMap CodeExpressionKey
    , _cmpOnCollisionExpr   :: EntityMap CodeExpressionKey
    , _cmpScriptData        :: EntityMap Dynamic
    , _cmpParent            :: EntityMap EntityID
    , _cmpRigidBody         :: EntityMap RigidBody
    , _cmpMass              :: EntityMap GLfloat
    , _cmpSpring            :: EntityMap SpringConstraint
    , _cmpPhysicsProperties :: EntityMap [PhysicsProperties]
    , _cmpPdPatch           :: EntityMap Patch
    , _cmpSoundSource       :: EntityMap OpenALSource
    , _cmpAttachment        :: EntityMap Attachment
    , _cmpLifetime          :: EntityMap Lifetime
    , _cmpAnimationColor    :: EntityMap (Animation (V4 GLfloat))
    , _cmpAnimationSize     :: EntityMap (Animation (V3 GLfloat))
    , _cmpConstraint        :: EntityMap Constraint
    , _cmpDrag              :: EntityMap Drag
    , _cmpOnDrag            :: EntityMap OnDrag
    }

newComponents :: Components
newComponents = Components
    { _cmpName              = mempty
    , _cmpPose              = mempty
    , _cmpSize              = mempty
    , _cmpShape             = mempty
    , _cmpColor             = mempty
    , _cmpOnStart           = mempty
    , _cmpOnUpdate          = mempty
    , _cmpOnCollision       = mempty
    , _cmpOnStartExpr       = mempty
    , _cmpOnUpdateExpr      = mempty
    , _cmpOnCollisionExpr   = mempty
    , _cmpScriptData        = mempty
    , _cmpParent            = mempty
    , _cmpRigidBody         = mempty
    , _cmpMass              = mempty
    , _cmpSpring            = mempty
    , _cmpPhysicsProperties = mempty
    , _cmpPdPatch           = mempty
    , _cmpSoundSource       = mempty
    , _cmpAttachment        = mempty
    , _cmpLifetime          = mempty
    , _cmpAnimationColor    = mempty
    , _cmpAnimationSize     = mempty
    , _cmpConstraint        = mempty
    , _cmpDrag              = mempty
    , _cmpOnDrag            = mempty
    }

data Entity = Entity
    { _entName              :: !String
    , _entSize              :: !(V3 GLfloat)
    , _entShape             :: !(ShapeType)
    , _entPose              :: !(Pose GLfloat)
    , _entColor             :: !(V4 GLfloat)
    , _entPhysicsProperties :: ![PhysicsProperties]
    , _entChildren          :: ![Entity]
    , _entMass              :: !Float
    , _entLifetime          :: !(Maybe Float)
    , _entPdPatch           :: !(Maybe FilePath)
    , _entOnStart           :: !(Maybe FilePath)
    , _entOnUpdate          :: !(Maybe FilePath)
    , _entOnCollision       :: !(Maybe FilePath)
    } deriving (Show, Generic)

newEntity :: Entity
newEntity = Entity
    { _entName              = "NewEntity"
    , _entSize              = V3 1 1 1
    , _entShape             = NoShape
    , _entPose              = newPose
    , _entColor             = V4 1 1 1 1
    , _entPhysicsProperties = []
    , _entChildren          = []
    , _entMass              = 1
    , _entLifetime          = Nothing
    , _entPdPatch           = Nothing
    , _entOnStart           = Nothing
    , _entOnUpdate          = Nothing
    , _entOnCollision       = Nothing
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

entityJSONOptions :: Options
entityJSONOptions = defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON Entity where
    parseJSON = genericParseJSON entityJSONOptions
instance ToJSON Entity where
    toJSON    = genericToJSON entityJSONOptions

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


