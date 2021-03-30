module Utility where


import Types

import Linear
import Control.Lens
import qualified SDL



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

-- * Helper function for drawing closed shapes

drawShape :: RealFrac a => SDL.Renderer -> [V2 a] -> IO ()
drawShape r = (\pts -> drawLines pts $ head pts) . map (SDL.P . fmap round)
    -- is this slower than SDL.drawLines??
    where
        drawLines [  ]       _      = pure ()
        drawLines [pt]       termPt = SDL.drawLine r pt termPt
        drawLines (p1 : p2 : pts) termPt =
            SDL.drawLine r p1 p2 >> drawLines (p2 : pts) termPt


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


