module Initialize where

import Types

import qualified Data.HashMap.Strict as HM
import Linear


initializeWorld :: World
initializeWorld =
    W
    { wShip = Ship (Position $ V2 10 10) (Velocity $ V2 5 5) (pi / 4 * 3) 3
    , wAsteroids =
        HM.fromList [(id, Asteroid 
                            id 
                            (Position $ V2 200 200) 
                            (Velocity $ V2 (5 * sin (fromIntegral id)) (5 * cos (fromIntegral $ id + 42))) 
                            0 
                            64
                      ) | id <- [0..999] ]
    , wBullets = HM.empty
    , wTime = 0
    , wScore = 0
    }
