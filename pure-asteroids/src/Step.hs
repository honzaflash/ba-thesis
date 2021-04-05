module Step where


import Types
import Input ( InputState )
import Step.Ship ( stepShip )
import Step.Asteroids ( stepAsteroids )
import Step.Bullets

import qualified Data.HashMap.Strict as HM
import Control.Lens



stepWorld :: Time -> InputState -> World -> (WorldEvents, World)
stepWorld deltaTime input oldW =
    let
        (eventsS, newShip) = stepShip deltaTime input oldW $ oldW ^. wShip
        (eventsB, newBullets) = stepBullets deltaTime input oldW $ oldW ^. wBullets
        -- (eventsU, newUfo) = updateUfos TODO
        (eventsScr, newScore) = (mempty, oldW ^. wScore)
    in
        (,) (eventsS <> eventsB <> eventsScr) $
        oldW
            & wShip      .~ newShip
            & wAsteroids %~ stepAsteroids deltaTime
            & wBullets   .~ newBullets
            -- & wUfos
            & wTime      %~ (+ deltaTime)
            & wScore     .~ newScore







