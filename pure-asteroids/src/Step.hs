module Step where


import Types

import Linear
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict ((!))
import Control.Lens


-- TODO  add keyboard events machinery -> different module
stepWorld :: Time -> WorldEvents -> World -> (WorldEvents, World)
stepWorld deltaTime events oldW =
    let
        -- TODO  recieving and sending events is chaos
        (eventsS, newShip) = updateShip deltaTime oldW $ oldW ^. wShip
        astEvents = events ^. forAsteroids
        newAsteroids = updateAsteroids deltaTime astEvents $ oldW ^. wAsteroids
        (eventsB, newBullets) = (mempty, oldW ^. wBullets)
        newTime = oldW ^. wTime
        -- (eventsU, newUfo) = updateUfos TODO
        newScore = oldW ^. wScore
    in
        (,) (eventsS <> eventsB)
        oldW
        { _wShip = newShip
        , _wAsteroids = newAsteroids
        , _wBullets = newBullets
        , _wTime = newTime
        , _wScore = newScore
        }


updateShip :: Time -> World -> Ship -> (WorldEvents, Ship)
updateShip dT w oldS =
    (,) mempty
    oldS
    { _sPosition = move dT (oldS ^. sVelocity) (oldS ^. sPosition)
    , _sVelocity = Velocity $ V2 1 1 -- TODO thrust function
    , _sAngle    = 0 -- TODO steer function
    }

    where
        collison = undefined -- foldl /genEvents/ $ wAsteroids w


updateAsteroids :: Time -> [AsteroidEvent] -> Asteroids -> Asteroids
updateAsteroids dT events oldAs =
    moveAsteroids dT $ foldl resolveDestruction oldAs events


resolveDestruction :: Asteroids -> AsteroidEvent -> Asteroids
resolveDestruction asteroids (BreakE id)
    | minAsteroidSize == (asteroids ! id) ^. aSize = break id asteroids 
    | otherwise = HM.delete id asteroids
    where
        break id asteroids =
            let (Asteroid _ pos vel _ size) = asteroids ! id
                part1 =
                    Asteroid
                    { _aId = id
                    , _aPosition = pos
                    , _aVelocity = vel & vVect %~ (\v -> v + perp v)
                    , _aAngle = 0
                    , _aSize = size `div` 2
                    }
                part2 =
                    Asteroid
                    { _aId = (+1) $ maximum $ HM.keys asteroids
                    , _aPosition = pos
                    , _aVelocity = vel & vVect %~ (\v -> v - perp v)
                    , _aAngle = 0
                    , _aSize = size `div` 2
                    }
            in insertAsteroid part2 $ insertAsteroid part1 asteroids

        insertAsteroid a = HM.insert (a ^. aId) a


moveAsteroids :: Time -> Asteroids -> Asteroids
moveAsteroids dT = HM.map (stepAsteroid dT)
    where
        stepAsteroid dT a =
            a & aPosition %~ move dT (a ^. aVelocity)


-- returns new position
move :: Time -> Velocity -> Position -> Position
move dT (Velocity v) = over pVect (+ fromIntegral dT / 100 *^ v)
    -- TODO wrap
