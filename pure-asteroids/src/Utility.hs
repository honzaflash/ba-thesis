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
    [ sPos - (17 *^ facing) + (17 *^ perp facing) -- left
    , sPos + (25 *^ facing)
    , sPos - (17 *^ facing) - (17 *^ perp facing) -- right
    , sPos - (8 *^ facing)
    ]
    where
        sPos = s ^. sPosition . pVect
        facing = angle $ s ^. sAngle


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

