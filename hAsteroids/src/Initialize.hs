module Initialize
( initializeGame
, spawnNewAsteroidWave
) where


import Components
import Resources
import Utility

import Apecs
import Control.Monad ( void, replicateM_ )
import Linear ( distance, V2(V2) )



initializeGame :: SystemWithResources ()
initializeGame = do
    newEntity ( Ship 0
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
            void $ newEntity (Asteroid 80, pos, vel)
        set global $ WaveNumber $ n + 1 -- increment wave number
    where
        safePosition shipPos = do
            Position pos <- askForRandPos
            if distance pos shipPos > 200
                then pure $ Position pos
                else safePosition shipPos

