{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Step where

import Components
import Utility

import Apecs
import Linear
import Foreign.C.Types (CDouble)
import Control.Monad (void)


stepScene :: IO Position -> IO Velocity -> System' ()
stepScene randomPosition randomVelocity = do
    cmap stepKinetics
    cmap decelerateShip
    cmapM_ collisions
    cmapM ageBullets
    spawnNewAsteroids randomPosition randomVelocity


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

collisions :: (Asteroid, Position, Entity) -> System' ()
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

ageBullets :: Bullet -> System' (Maybe Bullet)
ageBullets (Bullet age) = pure $
    if age > 0 then Just $ Bullet (age - 1) else Nothing

spawnNewAsteroids :: IO Position -> IO Velocity -> System' ()
spawnNewAsteroids randomPosition randomVelocity = do
    ran <- liftIO randomPosition
    let (Position (V2 x y)) = ran
    if x * y < 400
        then do
               pos <- liftIO randomPosition
               vel <- liftIO randomVelocity
               void $ newEntity (Asteroid 80, pos, vel)
        else pure ()

