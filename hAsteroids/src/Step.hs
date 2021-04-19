{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Step where


import Components
import Resources
import Utility

import Apecs
import Linear
import Foreign.C.Types (CDouble)
import Control.Monad (void)



stepScene :: Time -> SystemWithResources ()
stepScene dT = do
    cmap stepKinetics
    cmap decelerateShip
    cmapM_ collisions
    cmapM ageBullets
    spawnNewAsteroids


stepKinetics :: Kinetic -> Position
stepKinetics (Position p, Velocity v) = Position $ wrap $ p + v
    where
        wrap :: V2 CDouble -> V2 CDouble
        wrap (V2 x y) = V2 (x `wrapAxis` fromIntegral windowWidth) (y `wrapAxis` fromIntegral windowHeight)
        wrapAxis a m
          | a < (-50)  = a + m + 100
          | a > m + 50 = a - m - 100
          | otherwise  = a


decelerateShip :: (Ship, Velocity) -> Velocity
decelerateShip (_, Velocity v) = Velocity $ v ^* 0.965


collisions :: (Asteroid, Position, Entity) -> SystemWithResources ()
collisions (Asteroid size, Position aPos, aEty) = do
    cmapM_ $ \(Bullet _, Position bPos, bEty :: Entity) ->
                if distance aPos bPos < fromIntegral size / 2
                    then do
                           destroy bEty $ Proxy @(Bullet, Kinetic)
                           destroy aEty $ Proxy @(Asteroid, Kinetic)
                           modify global $ \(Score s) -> Score $ s + 100
                           (Score s) <- get global
                           liftIO $ putStrLn $ "Score: " ++ show s
                    else pure ()
    cmapM $ \(Ship _, Position sPos) ->
                if distance aPos sPos < 50 + fromIntegral size / 2
                    then do
                           destroy aEty $ Proxy @(Asteroid, Kinetic)
                           modify global $ \(Score _) -> Score 0
                           liftIO $ putStrLn "Score: 0"
                    else pure ()


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

