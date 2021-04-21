{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE MultiWayIf                 #-}

module Step where


import Components
import Resources
import Initialize (spawnNewAsteroidWave)
import Utility

import Apecs
import Linear
import Foreign.C.Types (CDouble)
import Control.Monad ( void, when, replicateM_ )



-- | Main step function, updates the state of everything
stepScene :: Time -> SystemWithResources ()
stepScene dT = do
    loopState <- get global
    when (loopState == Playing) $ do
        cmap $ stepKinetics dT -- moving everything that has velocity and position
        cmap $ decelerateShip dT
        cmapM $ stepShipState dT
        cmapM_ $ ufosShoot dT
        cmapM_ collisions
        spawnUfos
        spawnNewAsteroidWaveIfCleared
        -- spawnNewAsteroids -- new wave after the last one is destroyed
        modify global $ \(WaveTime t) -> WaveTime $ t + dT
        cmap $ \(Ttl t) -> Ttl $ t - dT -- decrement time to live
        destroyDeadBullets
        destroyDeadUfos
    where waveCleared = pure True :: IO Bool


-- | move everything that has Position and Velocity component
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


-- | detect and handle collisions
collisions :: (Asteroid, Position, Entity) -> SystemWithResources ()
collisions (Asteroid size, Position aPos, aEty) = do
    cmapM_ $ \(Bullet _, Position bPos, bEty :: Entity) ->
                when (distance aPos bPos < fromIntegral size / 2) $
                    do
                        destroy bEty $ Proxy @(Bullet, TimeToLive, Kinetic)
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


ufosShoot :: Time -> (Ship, Position, Velocity) -> SystemWithResources ()
ufosShoot dT (Ship _, Position shipPos, Velocity shipVel) =
    cmapM $ \(Ufo tts size, Position ufoPos) ->
        if tts > 0
            then pure (Ufo (tts - dT) size)
            else do
                    let aimingAlgo =
                            case size of
                                SmallSaucer -> predictiveShooting
                                LargeSaucer -> simpleShooting
                    let vel = ufoBulletSpeed *^ angle (aimingAlgo ufoPos shipPos shipVel)
                    newEntity (Bullet ShotByUfo
                              , Position ufoPos
                              , Velocity vel
                              , Ttl initUfoBulletTtl
                              )
                    pure (Ufo 1000 size)


-- | Predicts ship's position based on its current velocity
--   and returns an angle for the bullet's velocity
predictiveShooting :: (Floating a, RealFrac a, Ord a) => V2 a -> V2 a -> V2 a -> a
predictiveShooting uPos sPos sVel =
    unangle (sPos - uPos) + asin (norm sVel * sin beta / ufoBulletSpeed)
                   -- law of sines
    where
        beta = unangle (uPos - sPos) - unangle sVel


-- | Returns an angle for the bullet's velocity
--   based on the ship's current position
simpleShooting :: (Floating a, RealFrac a, Ord a) => V2 a -> V2 a -> V2 a -> a
simpleShooting uPos sPos _ =
    snapToEights $ unangle (sPos - uPos)
    where
        snapToEights alfa =
            fromIntegral (round (alfa / 2 / pi * 8) :: Int) / 8 * 2 * pi


decelerateShip :: Time -> (Ship, Velocity) -> Velocity
decelerateShip dT (_, Velocity v) =
    Velocity $ v ^* (0.975 ** (fromIntegral dT / fromIntegral targetDeltaTime))


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


spawnNewAsteroidWaveIfCleared :: SystemWithResources ()
spawnNewAsteroidWaveIfCleared = do
    count <- cfold (\count (Asteroid _) -> count + 1) 0
    when (count == 0) $ do
        set global $ WaveTime 0
        spawnNewAsteroidWave


spawnUfos :: SystemWithResources ()
spawnUfos = do
    r1 <- askForRandNum
    WaveTime t <- get global
    let spawnChance = 0.03 + 0.000001 * fromIntegral t
    when (t > 15000 && r1 < spawnChance) $
        do
            r2 <- askForRandNum
            let size = if r2 < 30 then SmallSaucer else LargeSaucer
            (Position (V2 _ posY)) <- askForRandPos
            void $ newEntity ( Ufo 1100 size
                             , Position $ V2 (-20) posY
                             , Velocity $ V2 10 0
                             , Ttl 11000
                             )


-- | Delete ufo if its time to live is less than 1
destroyDeadUfos :: SystemWithResources ()
destroyDeadUfos =
    cmapM_ $ \(Ufo _ _, Ttl t, uEty) ->
        when (t < 0) $ destroy uEty $ Proxy @(Ufo, TimeToLive, Kinetic)


-- | Delete bullets if its time to live is less than 1
destroyDeadBullets :: SystemWithResources ()
destroyDeadBullets =
    cmapM_ $ \(Bullet _, Ttl t, bEty) ->
        when (t < 0) $ destroy bEty $ Proxy @(Bullet, TimeToLive, Kinetic)

