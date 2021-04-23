{-# LANGUAGE TypeApplications           #-}
module Initialize
( initializeGame
, spawnNewAsteroidWave
, resetWorld
) where


import Components
import Resources
import Utility

import Apecs
import Control.Monad ( void, replicateM_ )
import Linear ( distance, V2(V2) )



initializeGame :: SystemWithResources ()
initializeGame = do
    newEntity ( Ship $ pi * 3 / 2
              , Position $ V2 (windowWidthF / 2) (windowHeightF / 2)
              , Velocity $ V2 0 0
              )
    spawnNewAsteroidWave


-- | Spawn safely a wave of asteroids
--   and increment the wave number
spawnNewAsteroidWave :: SystemWithResources ()
spawnNewAsteroidWave =
    cmapM_ $ \(Ship _, Position shipPos) -> do
        WaveNumber n <- get global
        replicateM_ (n + 4) $ do
            pos <- safePosition shipPos
            vel <- askForRandVel
            void $ newEntity (Asteroid initAsteroidSize, pos, vel)
        set global $ WaveNumber $ n + 1 -- increment wave number
    where
        safePosition shipPos = do
            Position pos <- askForRandPos
            if distance pos shipPos > 200
                then pure $ Position pos
                else safePosition shipPos


resetWorld :: SystemWithResources ()
resetWorld = do
    cmapM_ $ \(Asteroid _, aEty) -> destroy aEty $ Proxy @AsteroidComponents
    cmapM_ $ \(Bullet _, bEty) -> destroy bEty $ Proxy @BulletComponents
    cmapM_ $ \(Ufo _ _, uEty) -> destroy uEty $ Proxy @UfoComponents
    set global ((mempty, mempty, mempty, mempty, mempty, mempty, mempty, mempty)
                :: AllGlobals)
    initializeGame

