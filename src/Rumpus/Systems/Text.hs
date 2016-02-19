module Rumpus.Systems.Text where
import PreludeExtra hiding (Key)
import Data.ECS
import TinyRick
import Graphics.GL.Freetype

data TextBlock = TextBlock { _tbTextRenderer :: TextRenderer }
makeLenses ''TextBlock
defineComponentKeyWithType "Text" [t|TextBlock|]
