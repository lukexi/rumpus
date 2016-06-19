module Door where
import Rumpus

dW, dH :: Num a => a
dH = 2
dW = 1
dD = 0.1

doorColor = colorHSL 0.1 0.1 0.09

startTransitionAnimation = do
   let cubeW = 0.1
   let xs = floor $ fromIntegral dW / cubeW
       ys = floor $ fromIntegral dH / cubeW

   let cubePositions = [V2 x y | x <- [0..xs - 1], y <- [0..ys - 1]]
   forM_ cubePositions $ \(V2 xI yI) -> do
       hue <- randomRange (0,1)
       let x = fromIntegral xI * cubeW - (dW / 2) + cubeW / 2
           y = fromIntegral yI * cubeW - (dH / 2) + cubeW / 2
       spawnChild $ do
           myPose ==> position (V3 x y 0)
           mySize ==> realToFrac cubeW
           myShape ==> Cube
           myColor ==> doorColor
           myStart ==> do
               animateColor doorColor (colorHSL hue 0.3 0.5) 0.3
               setDelayedAction (0.3 + realToFrac hue) $ do
                   animatePositionTo (V3 x y -500) 1
start :: Start
start = do

    (mSceneName, sceneHue) <- getState (Just "New Home", 0)
    let sceneName = fromMaybe "New Scene" mSceneName

    myCodeHidden ==> True
    setShape Cube
    setSize $ V3 dW dH dD
    myBody ==> Animated
    myPose ==> positionRotation
        (V3 -1 1 0)
        (axisAngle (V3 0 1 0) (pi / 2))
    myColor ==> doorColor
    doorID <- ask
    -- Plaque
    let plaqueH = dH * 0.3
    spawnChild $ do
        myShape ==> Cube
        mySize  ==> V3 0.4 0.1 (dD + 0.01)
        myColor ==> colorHSL sceneHue 0.35 0.12
        myPose  ==> position $ V3 0 plaqueH 0
    spawnChild $ do
        myText ==> sceneName
        mySize ==> 0.05
        myPose ==> position $ V3 0 plaqueH ((dD + 0.02) / 2)

    let knobColor = colorHSL sceneHue 0.35 0.5
    doorKnob <- spawnChild $ do
        myShape        ==> Sphere
        mySize         ==> 0.1
        myBody         ==> Animated
        myColor        ==> knobColor
        myDragOverride ==> True
        myDragBegan    ==> do
            removeComponent myDragBegan
            inEntity doorID $ do
                removeComponent myShape
                startTransitionAnimation
                setDelayedAction 1 $
                    transitionToSceneOverTime sceneName 1

    -- Knob shaft
    spawnChildOf doorKnob $ do
        myColor ==> knobColor
        mySize  ==> V3 0.02 0.02 0.1
        myPose  ==> position $ V3 0 0 -0.05
        myShape ==> Cube

    attachEntity doorKnob
        (position $ V3 (dW * 0.33) 0 (dD + 0.03))

    let fW = 0.1
        frameColor = colorHSL sceneHue 0.5 0.5
    spawnChild_ $ do
       myShape ==> Cube
       mySize ==> V3 fW dH dD
       myPose ==> position (V3 (dW / 2 + fW / 2) 0 0)
       myColor ==> frameColor

    spawnChild_ $ do
       myShape ==> Cube
       mySize ==> V3 fW dH dD
       myPose ==> position (V3 (-dW / 2 - fW / 2) 0 0)
       myColor ==> frameColor

    spawnChild_ $ do
       mySize ==> V3 (dW + fW * 2) fW dD
       myPose ==> position (V3 0 (dH/2 + fW / 2) 0)
       myColor ==> frameColor
       myShape ==> Cube
