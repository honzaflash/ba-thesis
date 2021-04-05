module Step.Common where

import Types
import Utility

import Linear
import Control.Lens

-- | Returns new position
move :: Time -> Velocity -> Position -> Position
move dT (Velocity v) = over pVect addAndWrap
    where
        addAndWrap = wrapPos . (+ fromIntegral dT / 100 *^ v)
        wrapPos (V2 x y) = V2 (wrapAxis x windowWidthF) (wrapAxis y windowHeightF)
        wrapAxis a limit
            | a < (- 40)     = a + limit + 80
            | a > limit + 40 = a - limit - 80
            | otherwise      = a
