module Step where


import Types

import Linear
-- import Control.Lens -- TODO  transfer to using lenses!
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict ((!))


-- TODO  add keyboard state... what about single key presses? <- shooting
stepWorld :: Time -> WorldEvents -> World -> World
stepWorld deltaTime events oldW =
    let astEvents = forAsteroids events in
        oldW
        { wShip = updateShip deltaTime $ wShip oldW
        , wAsteroids = updateAsteroids deltaTime astEvents $ wAsteroids oldW
        , wBullets = wBullets oldW
        , wTime = wTime oldW
        , wScore = wScore oldW
        }


updateShip :: Time -> Ship -> Ship
updateShip dT oldS =
    oldS
    { shipPosition = move dT (shipPosition oldS) (shipVelocity oldS)
    , shipVelocity = Velocity $ V2 0 0 -- propel function
    , shipAngle    = 0 -- steer function
    }


updateAsteroids :: Time -> [AsteroidEvent] -> Asteroids -> Asteroids 
updateAsteroids dT events oldAs =
    stepAsteroids dT $ foldl resolveDestruction oldAs events


resolveDestruction :: Asteroids -> AsteroidEvent -> Asteroids
resolveDestruction asteroids (Destroy id)
    | minAsteroidSize == astSize (asteroids ! id) = break id asteroids 
    | otherwise = HM.delete id asteroids
    where
        break id asteroids =
            let (Asteroid _ pos vel _ size) = asteroids ! id
                part1 =
                    Asteroid
                    { astId = id
                    , astPosition = pos
                    , astVelocity = kmap (\v -> v + perp v) vel
                    , astAngle = 0
                    , astSize = size `div` 2
                    }
                part2 =
                    Asteroid
                    { astId = (+1) $ maximum $ HM.keys asteroids
                    , astPosition = pos
                    , astVelocity = kmap (\v -> v - perp v) vel
                    , astAngle = 0
                    , astSize = size `div` 2
                    }
            in insertAsteroid part2 $ insertAsteroid part1 asteroids

        insertAsteroid a = HM.insert (astId a) a


stepAsteroids :: Time -> Asteroids -> Asteroids
stepAsteroids dT = HM.map (stepAsteroid dT)
    where
        stepAsteroid dT a =
            a { astPosition = move dT (astPosition a) (astVelocity a) }


-- returns new position
move :: Time -> Position -> Velocity -> Position
move dT position (Velocity v) = kmap (+ fromIntegral dT *^ v) position
