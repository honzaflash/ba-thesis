module Initialize where


import Types
import Utility ( windowHeightF, windowWidthF )

import qualified Data.HashMap.Strict as HM
import Linear



initializeWorld :: World
initializeWorld =
    World
    { _wShip = Ship (Position $ V2 (windowHeightF/2) (windowWidthF/2)) (Velocity $ V2 0 0) (pi / 4 * 3) 3 ShipAlive
    , _wAsteroids = safeRandomAsteroidSpawn 4
    , _wBullets = HM.empty
    , _wUfos = HM.empty
    , _wWaveTime = 0
    , _wWavePause = 0
    , _wWaveNum = 1
    , _wScore = Score 0
    }


safeRandomAsteroidSpawn :: Int -> Asteroids
safeRandomAsteroidSpawn n = 
        HM.fromList [(id, Asteroid 
                            id
                            (Position $ V2 350 $ fromIntegral (id `mod` 500 + 100)) 
                            (Velocity $ V2 (5 * sin (fromIntegral id)) (5 * cos (fromIntegral $ id + 42))) 
                            0
                            initAsteroidSize
                      ) | id <- [0..n] ]


