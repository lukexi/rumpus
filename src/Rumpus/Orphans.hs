{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rumpus.Orphans where
import Data.Aeson.Types
import PreludeExtra
-----------------
-- JSON instances
-----------------

instance FromJSON a => FromJSON (V4 a)
instance FromJSON a => FromJSON (V3 a)
instance FromJSON a => FromJSON (V2 a)
instance FromJSON a => FromJSON (Quaternion a)
instance ToJSON a => ToJSON (V4 a)
instance ToJSON a => ToJSON (V3 a)
instance ToJSON a => ToJSON (V2 a)
instance ToJSON a => ToJSON (Quaternion a)

poseJSONOptions :: Options
poseJSONOptions = defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON a => FromJSON (Pose a) where
    parseJSON = genericParseJSON poseJSONOptions
instance ToJSON a => ToJSON (Pose a) where
    toJSON     = genericToJSON poseJSONOptions


