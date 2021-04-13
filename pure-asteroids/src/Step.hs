module Step 
( stepWorld
) where


import Types
import Input ( InputState )
import Step.Ship ( stepShip )
import Step.Asteroids ( stepAsteroids )
import Step.Ufos ( stepUfos )
import Step.Bullets ( stepBullets )
import Initialize ( safeRandomAsteroidsSpawn )
import Utility

import qualified Data.HashMap.Strict as HM
import Control.Lens



stepWorld :: Time -> InputState -> RandomStream Double -> World -> (WorldEvents, World)
stepWorld deltaTime input rand oldW =
    let
        (eventsS, newShip) = stepShip deltaTime input oldW $ oldW ^. wShip
        (eventsB, newBullets) = stepBullets deltaTime input oldW $ oldW ^. wBullets
        (eventsU, newUfos) = stepUfos deltaTime rand oldW $ oldW ^. wUfos
        (eventsScr, newScore) = (mempty, oldW ^. wScore)
    in
        (,) (eventsS <> eventsB <> eventsU <> eventsScr) $
        checkWave $
        oldW
            & wShip      .~ newShip
            & wAsteroids %~ stepAsteroids deltaTime
            & wBullets   .~ newBullets
            & wUfos      .~ newUfos
            & wWaveTime  +~ deltaTime
            & wScore     .~ newScore
    where
        checkWave w
            | null (w ^. wAsteroids) &&
                w ^. wWavePause <  2000 =
                          w
                           & wWavePause +~ deltaTime
                           & wWaveTime .~ 0
            | null (w ^. wAsteroids) &&
                w ^. wWavePause >= 2000 =
                          w
                           & wWavePause .~ 0
                           & wAsteroids .~ safeRandomAsteroidsSpawn
                                               (oldW ^. wWaveTime)
                                               (w ^. wShip)
                                               (w ^. wWaveNum + 4)
                           & wWaveNum +~ 1
            | otherwise = w

