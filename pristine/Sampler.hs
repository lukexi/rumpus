module Sampler where
import Rumpus
import qualified Data.Vector as V
import Data.String

toPdPathStyle path = do
    absolute <- liftIO (makeAbsolute path)
    return $ map (\c -> if c == '\\' then '/' else c) absolute

start :: Start
start = do
    setSynthPatch "Sampler.pd"

    thisID <- ask
    mStateFolder <- getSceneStateFolder
    forM_ mStateFolder $ \stateFolder -> do
        sampleFileName <- toPdPathStyle (stateFolder </> show thisID <.> "wav")
        sendSynth "sample-file" (fromString sampleFileName)

    let numSamples = 32 -- actual generated is 256
    children <- V.generateM numSamples $ \i -> do
        let x = fromIntegral i / 8 + 1
        spawnChild $ do
            myShape            ==> Cube
            mySize             ==> 1
            myColor            ==> V4 0.8 0.9 0.4 1
            myPose             ==> position (V3 x 0 0)
    mainID <- ask

    pose <- getPose
    button <- spawnChild $ do
        myShape           ==> Cube
        myBody            ==> Animated
        mySize            ==> 0.1
        -- Set initial pose so there's not a spurious collision
        myPose            ==> pose !*! position (V3 0 0.5 0)
        myDragOverride    ==> True
        myCollisionBegan  ==> \_ _ -> do
            hue <- randomRange (0,1)
            setColor $ colorHSL hue 0.8 0.4
            sendEntitySynth mainID "begin-recording" Bang

    attachEntity button (position $ V3 0 0.5 0)

    myUpdate          ==> do
        fftSample <- V.convert <$> readPdArray "sample-fft" 0 numSamples
        V.forM_ (V.zip children fftSample) $ \(childID, sample) -> do
            let val = sample
            inEntity childID $ do
                setSize (0.01 & _yz .~ realToFrac val * 0.1)
                myColor ==> colorHSL (realToFrac val) 0.8 0.4
        return ()
