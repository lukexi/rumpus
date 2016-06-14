module Door where
import Rumpus

start :: Start
start = do


    door <- spawnChild $ do
        myShape ==> Cube
        mySize ==> V3 0.6 1 0.2
        myBody ==> Animated
        myPose ==> positionRotation
            (V3 -1 1 0)
            (axisAngle (V3 0 1 0) (pi / 2))
        myColor ==> colorHSL 0.1 0.1 0.09

    spawnChildOf door $ do
        myShape ==> Cube
        mySize ==> V3 0.4 0.1 0.22
        myColor ==> colorHSL 0.9 0.35 0.12
        myPose ==> position $ V3 0 0.3 0
    spawnChildOf door $ do
        myText ==> "City Paint"
        mySize ==> 0.05
        myPose ==> position $ V3 0 0.3 0.12

    doorKnob <- spawnChildOf door $ do
        myShape ==> Sphere
        mySize  ==> 0.1
        myBody  ==> Animated
        myColor ==> colorHSL 0.2 0.35 0.5
    -- Knob shaft
    spawnChildOf doorKnob $ do
        myColor ==> colorHSL 0.2 0.35 0.5
        mySize  ==> V3 0.02 0.02 0.1
        myPose  ==> position $ V3 0 0 -0.05
        myShape ==> Cube

    attachEntityToEntity door doorKnob
        (position $ V3 0.2 0 0.175)
