module Superformula where
import Rumpus

-- https://en.wikipedia.org/wiki/Superformula
superformulaPolar a b m n1 n2 n3 phi =
    ( abs (cos (m * phi / 4) / a) ** n2
      +
      abs (sin (m * phi / 4) / b) ** n3
    ) ** (-1/n1)


-- theta            -pi <> pi
-- phi (latitude)   -pi/2 <> pi/2
sphereSuperformula theta phi
    a_1 b_1 m_1 n1_1 n2_1 n3_1
    a_2 b_2 m_2 n1_2 n2_2 n3_2
    = V3 x y z
    where
        r1 phi = superformulaPolar a_1 b_1 m_1 n1_1 n2_1 n3_1 phi
        r2 phi = superformulaPolar a_2 b_2 m_2 n1_2 n2_2 n3_2 phi
        x      = r1 theta * cos theta * r2 phi * cos phi
        y      = r1 theta * sin theta * r2 phi * cos phi
        z      = r2 phi * sin phi


start :: Start
start = do
    let n = 15
        formulaCenter = V3 0 1 0
        -- Mondo superformula
        (formulaSize, pointSize) = (5, 0.1)
        -- Mini superformula
--        (formulaSize, pointSize) = (0.15, 0.01)

    -- Show the 2D superformula
    -- visualize2D n

    let threeD = [(theta, phi)
                    | theta <- floats (-pi, pi) n
                    , phi   <- floats (-pi/2, pi/2) n]
    spawnChildren threeD $ \(theta, phi) -> do
        myShape         ==> Sphere
        myTransformType ==> AbsolutePose
        mySize          ==> pointSize
        myColor         ==> colorHSL (phi+theta) 0.5 0.5
        myUpdate ==> do
            n <- getNow
            let now = (sin n + 1) * 0.3 + 0.5
            let now = n
            setColor (colorHSL (phi*now + theta * now) 0.5 0.5)
            setPosition (
                sphereSuperformula (theta + now) (phi * now)
                    1 1 3 5 18 18
                    1 1 3 5 18 18
                * formulaSize
                + formulaCenter
                )

    return ()

visualize2D n = do
    let aSuperformula = superformulaPolar 1 1 3 5 18 18
    spawnChildren (floats01 n) $ \i -> do
        let theta  = remap (-pi) pi i
            r      = aSuperformula theta
            V2 x y = polarToCartesian (V2 (r*0.3) theta)
        myShape ==> Cube
        mySize  ==> 0.01
        myPose  ==> position (V3 x y -0.5)

