module Step where


import Types

import Linear
-- import Control.Lens -- TODO  transfer to using lenses
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict ((!))


-- TODO  add keyboard events machinery -> different module
stepWorld :: Time -> WorldEvents -> World -> (WorldEvents, World)
stepWorld deltaTime events oldW =
    let
        -- TODO  recieving and sending events is chaos
        (eventsS, newShip) = updateShip deltaTime oldW $ wShip oldW
        astEvents = forAsteroids events
        newAsteroids = updateAsteroids deltaTime astEvents $ wAsteroids oldW
        (eventsB, newBullets) = (mempty, wBullets oldW)
        newTime = wTime oldW
        -- (eventsU, newUfo) = updateUfos TODO
        newScore = wScore oldW
    in
        (,) (eventsS <> eventsB)
        oldW
        { wShip = newShip
        , wAsteroids = newAsteroids
        , wBullets = newBullets
        , wTime = newTime
        , wScore = newScore
        }


updateShip :: Time -> World -> Ship -> (WorldEvents, Ship)
updateShip dT w oldS =
    (,) mempty
    oldS
    { shipPosition = move dT (shipPosition oldS) (shipVelocity oldS)
    , shipVelocity = Velocity $ V2 1 1 -- TODO thrust function
    , shipAngle    = 0 -- TODO steer function
    }

    where
        collison = undefined -- foldl /genEvents/ $ wAsteroids w


updateAsteroids :: Time -> [AsteroidEvent] -> Asteroids -> Asteroids
updateAsteroids dT events oldAs =
    moveAsteroids dT $ foldl resolveDestruction oldAs events


resolveDestruction :: Asteroids -> AsteroidEvent -> Asteroids
resolveDestruction asteroids (BreakE id)
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


moveAsteroids :: Time -> Asteroids -> Asteroids
moveAsteroids dT = HM.map (stepAsteroid dT)
    where
        stepAsteroid dT a =
            a { astPosition = move dT (astPosition a) (astVelocity a) }


-- returns new position
move :: Time -> Position -> Velocity -> Position
move dT position (Velocity v) = kmap (+ fromIntegral dT / 100 *^ v) position
    -- TODO wrap
