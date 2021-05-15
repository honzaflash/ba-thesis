module Step 
( stepWorld
) where


import Types
import Input ( InputState )
import Step.Ship ( stepShip )
import Step.Asteroids ( stepAsteroids )
import Step.Ufos ( stepUfos )
import Step.Bullets ( stepBullets )
import Step.Score ( stepScore )
import Initialize ( safeRandomAsteroidsSpawn )
import Utility

import qualified Data.HashMap.Strict as HM
import Control.Lens



-- | Update the world simulating physics and reacting to input
stepWorld
    :: Time -- the frame delta time
    -> InputState -- current state of input
    -> RandomStream Double -- infinite list of random doubles
    -> World -- old world state
    -> (WorldEvents, World) -- new world state and events
stepWorld dT input rand oldW =
    let
        (eventsS, newShip)    = stepShip dT input oldW $ oldW ^. wShip
        (eventsB, newBullets) = stepBullets dT input oldW $ oldW ^. wBullets
        (eventsU, newUfos)    = stepUfos dT rand oldW $ oldW ^. wUfos
        (eventsScr, newScore) = stepScore dT $ oldW ^. wScore
    in
        (,) (eventsS <> eventsB <> eventsU <> eventsScr) $
        checkWave $
        oldW
          & wShip      .~ newShip
          & wAsteroids %~ stepAsteroids dT
          & wBullets   .~ newBullets
          & wUfos      .~ newUfos
          & wWaveTime  +~ dT
          & wScore     .~ newScore

    where
        checkWave w
            | null (w ^. wAsteroids) &&
                w ^. wWavePause <  2000 =
                        w
                         & wWavePause +~ dT
                         & wWaveTime .~ 0
            | null (w ^. wAsteroids) &&
                w ^. wWavePause >= 2000 =
                        w
                         & wWavePause .~ 0
                         & wAsteroids .~ safeRandomAsteroidsSpawn
                                            (oldW ^. wScore . sValue) -- Score as a seed
                                            (w ^. wShip)
                                            (w ^. wWaveNum + 4)
                         & wWaveNum +~ 1
            | otherwise = w

