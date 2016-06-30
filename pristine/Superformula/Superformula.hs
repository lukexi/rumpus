module Superformula where
import Rumpus


start :: Start
start = do
    let n = 50

    -- 2D vis
    --let aSuperformula = superformulaPolar 1 1 3 5 18 18
    --spawnChildren (floats01 n) $ \i -> do
    --    let theta = remap (-pi) pi i
    --        r     = aSuperformula theta
    --        V2 x y = polarToCartesian (r*0.3) theta
    --    myShape ==> Cube
    --    mySize  ==> 0.01
    --    myPose  ==> position (V3 x y -0.5)

    let threeD = [(theta, phi)
                    | theta <- floats (-pi, pi) n
                    , phi   <- floats (-pi/2, pi/2) n]
    spawnChildren threeD $ \(theta, phi) -> do
        let pos = sphereSuperformula theta phi
                1 1 3 5 18 18
                1 2 2.53 0.8 18 13
        --printIO pos
        myShape ==> Cube

        mySize  ==> 0.05
        myPose  ==> position (pos *0.1+ V3 0 1 0)
        myColor ==> colorHSL (phi+theta) 0.5 0.5

    return ()


-- Build a morphing tunnel with this

-- https://en.wikipedia.org/wiki/Superformula

remap low hi n = low + range * n
    where range = (hi - low)

floats01 n = [ fromIntegral i / fromIntegral n | i <- [0..n] ]

floats (low, hi) n = remap low hi <$> floats01 n

polarToCartesian r theta = V2 (r * cos theta) (r * sin theta)

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
