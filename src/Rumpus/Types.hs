module Rumpus.Types where
import Data.ECS

type HandEntityID = EntityID
{-
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



entityJSONOptions :: Options
entityJSONOptions = defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON Entity where
    parseJSON = genericParseJSON entityJSONOptions
instance ToJSON Entity where
    toJSON    = genericToJSON entityJSONOptions
-}




