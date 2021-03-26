module Initialize where

import Types

import qualified Data.HashMap.Strict as HM
import Linear


initializeWorld :: World
initializeWorld =
    W
    { wShip = Ship (Position $ V2 0 0) (Velocity $ V2 5 5) (pi / 4 * 3)
    , wAsteroids =
        HM.fromList $ zip [1..] $ replicate 1000 $ Asteroid 1 (Position $ V2 100 100) (Velocity $ V2 5 5) 0 128
    , wBullets = HM.empty
    , wTime = 0
    , wScore = 0
    }
