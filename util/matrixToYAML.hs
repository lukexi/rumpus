--import Control.Lens
import Data.Yaml
import Data.ByteString.Char8 as BSC
import Linear.Extra

main = do

    let matrix :: M44 Float
        matrix = mkTransformation (axisAngle (V3 0 1 0) pi) (V3 11 12 13)
    BSC.putStrLn (encode matrix)