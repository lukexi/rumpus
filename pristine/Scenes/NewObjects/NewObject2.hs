module Foo where
import Rumpus
import Graphics.GL.TextBuffer

start :: Start
start = do
    setSize 0.5
 
    traverseM_ (getComponent myStartExpr) $ \codeExprKey -> 
        modifySystemState sysCodeEditor $ do
            cesCodeEditors . ix codeExprKey . cedCodeRenderer %=~
                updateMetrics . (txrScreenSize ?~ V2 100 100)

    thisID <- ask
    forM_ [(LeftHand, -1), (RightHand, 1)] $ \(whichHand, xOffset) -> do
        traverseM_ (viewSystem sysKeyboardHands (kbhKeyboard . at whichHand)) $ \containerID -> do
            runEntity containerID $ do
                setPose $ mkTransformation (axisAngle (V3 1 0 0) (pi/2)) (V3 (xOffset/2) 0 0)
                setParent thisID
 
    setRepeatingAction 0.1 $ do
        n <- getNow  
        setColor (hslColor n 0.5 0.5) 
     
    setColor (hslColor 0.4 0.8 0.1)