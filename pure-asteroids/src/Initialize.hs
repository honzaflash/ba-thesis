module Initialize where


import Types
import Utility

import qualified Data.HashMap.Strict as HM
import Linear
import Control.Lens



initializeWorld :: World
initializeWorld =
    World
    { _wShip      = initializeShip
    , _wAsteroids = safeRandomAsteroidsSpawn 420 initializeShip 4
    , _wBullets   = HM.empty
    , _wUfos      = HM.empty
    , _wWaveTime  = 0
    , _wWavePause = 0
    , _wWaveNum   = 1
    , _wScore     = Score 0
    }


initializeShip :: Ship
initializeShip =
    Ship
    { _sPosition = Position $ V2 (windowHeightF/2) (windowWidthF/2)
    , _sVelocity = Velocity $ V2 0 0
    , _sAngle    = pi / 4 * 3
    , _sLives    = 3
    , _sState    = ShipAlive
    }


safeRandomAsteroidsSpawn :: Int -> Ship -> Int -> Asteroids
safeRandomAsteroidsSpawn seed ship n =
    HM.fromList $ take n $ zip [0..] $
        zipWith3 asteroidize [0..] streamPos streamVel
    where
        asteroidize id pos vel =
            Asteroid
            { _aId       = id
            , _aPosition = pos
            , _aVelocity = vel
            , _aSize     = initAsteroidSize
            }
            
        streamPos = map Position $ filter notTooClose streamPosVect
        streamPosVect = randV2StreamGen (0, windowWidthF) (0, windowHeightF) seed
        notTooClose = (200 <) . distance (ship ^. sPosition . pVect)

        streamVel = map (Velocity . minSize) streamVelVect
        minSize vect = vect + 2 *^ signorm vect
        streamVelVect = randV2StreamGen (-3, 3) (-3, 3) (97 * seed)

