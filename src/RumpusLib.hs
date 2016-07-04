{-# LANGUAGE ViewPatterns #-}

module RumpusLib where
import PreludeExtra
import Data.List (isPrefixOf)

createNewTimer :: (RealFrac a, MonadIO m) => a -> m (TVar Bool)
createNewTimer ticksPerSec = liftIO $ registerDelay (floor $ (1/ticksPerSec) * 1000 * 1000)

checkTimer :: MonadIO m => TVar Bool -> m Bool
checkTimer = liftIO . atomically . readTVar

randomFrom :: MonadIO m => [a] -> m a
randomFrom list = do
    i <- randomRange (0, length list - 1)
    return (list !! i)

-- | Points distributed evenly in a sphere
-- (via http://www.softimageblog.com/archives/115)
goldenSectionSpiralPoints :: Int -> [V3 GLfloat]
goldenSectionSpiralPoints (fromIntegral -> n) =
    let inc = pi * (3 - sqrt 5)
        off = 2 / n
    in flip map [0..n] $ \k ->
        let y = k * off - 1 + (off / 2)
            r = sqrt (1 - y*y)
            phi = k * inc
        in V3 (cos phi * r) y (sin phi * r)

randomRange :: (Random a, MonadIO m) => (a, a) -> m a
randomRange = liftIO . randomRIO

positionRotation :: Num a => V3 a -> Quaternion a -> M44 a
positionRotation = flip mkTransformation

positionRotationScale :: Num a => V3 a -> Quaternion a -> V3 a -> M44 a
positionRotationScale pos rot scale = positionRotation pos rot !*! scaleMatrix scale

position :: Num a => V3 a -> M44 a
position  = translateMatrix
rotation :: (Epsilon a, Floating a) => V3 a -> a -> M44 a
rotation axis angle_ =
    rotationQ (axisAngle axis angle_)
rotationQ :: Num a => Quaternion a -> M44 a
rotationQ q = mkTransformation q 0

-- | Given a list of names like [NewObject1, NewObject3, NewObject17],
-- and their common prefix (in example, NewObject),
-- finds the successor name of the highest name.
-- Example list would return NewObject18.
findNextNumberedName :: String -> [String] -> String
findNextNumberedName name inList =
    let newObjects = filter (isPrefixOf name) inList
        existingNumbers = catMaybes $ map (readMaybe . drop (length name)) newObjects :: [Int]
        highest = if null existingNumbers then 0 else maximum existingNumbers
    in name ++ show (succ highest)


-- Quantization
-- | Quantizes a float to the nearest multiple of q
quantizeToF q x =
    let (quotient, remainder) = x `quotRemF` q
    in fromIntegral quotient * q + ((fromIntegral $ round (remainder / q)) * q)

-- | Quantizes an integer to the nearest multiple of q
quantizeToI q x =
    let (quotient, remainder) = x `quotRem` q
    in quotient * q + (round (remainder // q) * q)

-- | A version of quotRem that works on Floats rather than Ints
quotRemF x y = (quotient, remainder)
    where
        quotient  = floor (x / y)
        remainder = x - y * fromIntegral quotient

-- | Divide two Integrals to get a Floating
x // y = fromIntegral x / fromIntegral y

-- | Remap a 0-1 n to the range low,high
remap low hi n = low + range * n
    where range = (hi - low)

-- | Generate a linear progression of floats between 0.0-1.0
floats01 n = [ fromIntegral i / fromIntegral n | i <- [0..n] ]

-- | Generate a linear progression of floats between low and hi
floats (low, hi) n = remap low hi <$> floats01 n

-- | Converts polar to cartesian coordinates
polarToCartesian r theta = V2 (r * cos theta) (r * sin theta)
