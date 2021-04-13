module Utility where


import Types

import Linear
import Control.Lens
import qualified SDL
import System.Random
import Data.List ( unfoldr )



-- * Global constants

-- | Window dimensions
windowWidth, windowHeight :: Integral a => a
windowWidth = 1024
windowHeight = 768

windowWidthF, windowHeightF :: RealFloat a => a
windowWidthF = fromIntegral windowWidth
windowHeightF = fromIntegral windowHeight

-- | Target FPS and the computed time per frame
targetFPS, targetIterationTime :: Integral a => a
targetFPS = 60
targetIterationTime = 1000 `div` targetFPS


-- | Calculates the ship vertices' positions
shipPoints :: Ship -> [V2 Double]
shipPoints s =
    map (+ sPos)
        [ -17 *^ (facing - perp facing) -- left
        ,  25 *^  facing                -- front tip
        , -17 *^ (facing + perp facing) -- right
        , -8  *^  facing                -- back wedge point
        ]
    where
        sPos = s ^. sPosition . pVect
        facing = angle $ s ^. sAngle


-- | Calculates the ufos vertices' positions
ufoPoints :: Ufo -> [V2 Double]
ufoPoints u =
    map (+ uPos)
        [ V2 (-2 * size)   0
        , V2 (-    size) (-size)
        , V2       size  (-size)
        , V2 ( 2 * size)   0
        , V2       size    size
        , V2 (-    size)   size
        ]
    where
        uPos = u ^. uPosition . pVect
        size = case u ^. uSize of
                   SmallSaucer -> 10
                   LargeSaucer -> 20



-- * Pseudo-random generators

type RandomStream a = [a]

-- | Pure pseudo-random V2 generator
randV2StreamGen ::
    ( UniformRange a
    , RealFrac a
    )
    => (a, a)
    -> (a, a)
    -> Int
    -> [V2 a]
randV2StreamGen xRange yRange seed =
    let
        streamX = randStreamGen xRange seed
        streamY = randStreamGen yRange $ round $ head streamX
    in
        zipWith V2 streamX streamY


-- | Pure pseudo-random generator
randStreamGen :: UniformRange a => (a, a) -> Int -> [a]
randStreamGen range =
    unfoldr (Just . uniformR range) . mkStdGen

