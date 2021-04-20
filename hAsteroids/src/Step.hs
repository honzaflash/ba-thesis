{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Step where


import Components
import Resources
import Utility

import Apecs
import Linear
import Foreign.C.Types (CDouble)
import Control.Monad ( void, when )



stepScene :: Time -> SystemWithResources ()
stepScene dT = do
    loopState <- get global
    case loopState of
        Playing -> do
            cmap $ stepKinetics dT -- moving everything that has velocity and position
            cmap $ decelerateShip dT
            cmapM $ stepShipState dT
            cmapM_ collisions
            cmapM ageBullets
            spawnNewAsteroids
        _ -> pure ()


stepKinetics :: Time -> Kinetic -> Position
stepKinetics dT (Position p, Velocity v) =
    Position $ wrap $ p + v ^* fromIntegral dT / 100
    where
        wrap :: V2 CDouble -> V2 CDouble
        wrap (V2 x y) = V2
                          (x `wrapAxis` fromIntegral windowWidth)
                          (y `wrapAxis` fromIntegral windowHeight)
        wrapAxis a m
          | a < (-50)  = a + m + 100
          | a > m + 50 = a - m - 100
          | otherwise  = a


decelerateShip :: Time -> (Ship, Velocity) -> Velocity
decelerateShip dT (_, Velocity v) =
    Velocity $ v ^* (0.975 ** (fromIntegral dT / fromIntegral targetDeltaTime))


collisions :: (Asteroid, Position, Entity) -> SystemWithResources ()
collisions (Asteroid size, Position aPos, aEty) = do
    cmapM_ $ \(Bullet _, Position bPos, bEty :: Entity) ->
                when (distance aPos bPos < fromIntegral size / 2) $
                    do
                        destroy bEty $ Proxy @(Bullet, Kinetic)
                        destroy aEty $ Proxy @(Asteroid, Kinetic)
                        modify global $ \(Score s) -> Score $ s + 100
                        (Score s) <- get global
                        liftIO $ putStrLn $ "Score: " ++ show s
    cmapM_ $ \(Ship _, Position sPos, Velocity vel, sEty :: Entity, state :: ShipState) ->
                when (state == Alive && distance aPos sPos < 50 + fromIntegral size / 2) $
                    do
                        destroy aEty $ Proxy @(Asteroid, Kinetic)
                        modify global $ \(ShipLives x) ->
                            (ShipLives $ x - 1, Exploding 700)
                        set sEty (Velocity $ V2 0 0)
                        modify global $ \(ShipLives x) ->
                            if x == 0 then GameOver else Playing
                        liftIO $ putStrLn "Score: 0"


-- | update the ship state and modify ship components along the way
stepShipState :: Time -> (Ship, Entity, ShipState) -> SystemWithResources (Ship, ShipState)
stepShipState dT (Ship a, sEty, Exploding t)
    | t > 0     = pure (Ship $ a + 0.03 * fromIntegral dT, Exploding $ t - dT)
    | otherwise = do
        set sEty $ Position $ V2 (windowWidthF /2) (windowHeightF / 2)
        pure (Ship $ pi * 3 / 2, Respawning 1500)
stepShipState dT (Ship a, _, Respawning t)
    | t > 0     = pure (Ship a, Respawning $ t - dT)
    | otherwise = pure (Ship a, Alive)
stepShipState dT (Ship a, _, Alive) = pure (Ship a, Alive)


-- | decrement bullet's TTL and delete it if it's less than 1
ageBullets :: Bullet -> SystemWithResources (Maybe Bullet)
ageBullets (Bullet age) = pure $
    if age > 0 then Just $ Bullet (age - 1) else Nothing


spawnNewAsteroids :: SystemWithResources ()
spawnNewAsteroids = do
    randomPosition <- askForRandPosGen
    ran <- liftIO randomPosition
    let (Position (V2 x y)) = ran
    if x * y < 400
        then do
               pos <- liftIO randomPosition
               vel <- askForRandVel
               void $ newEntity (Asteroid 80, pos, vel)
        else pure ()

