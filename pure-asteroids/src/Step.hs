module Step where


import Types
import Utility ( windowWidthF, windowHeightF, shipPoints )

import Linear
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Data.Foldable ( fold )
import Input



-- TODO  add keyboard events machinery -> different module
stepWorld :: Time -> InputState -> World -> (WorldEvents, World)
stepWorld deltaTime input oldW =
    let
        (eventsS, newShip) = stepShip deltaTime input oldW $ oldW ^. wShip
        (eventsB, newBullets) = (mempty, oldW ^. wBullets)
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


stepShip :: Time -> InputState -> World -> Ship -> (WorldEvents, Ship)
stepShip dT input w oldS =
    (,) events $
    oldS
        & sPosition         %~ move dT (oldS ^. sVelocity)
        & sVelocity . vVect %~ thrust . deceleration
        & sAngle            %~ steer
        & sLives            %~ case events of
                                   (WorldEvents [] _ [] _) -> id
                                   _                       -> subtract 1

    where
        -- Event generation
        events = mempty
                    & forAsteroids .~ eventsForAsteroids
                    & forUfos      .~ eventsForUfos
                    -- TODO  forGameLoop = GameOver if lives == 0

        eventsForAsteroids =
            [ BreakE $ a ^. aId |
                a <- HM.elems $ w ^. wAsteroids,
                astCollision a ]
        astCollision a = any (isInside a) $ take 3 $ shipPoints oldS -- 4th doesn't need to be checked
        isInside a = (fromIntegral (a ^. aSize) >) . distance (a ^. aPosition . pVect)

        eventsForUfos = []

        -- Control functions
        thrust = if input ^. isHeldW 
                     then (+ thrustStrength *^ angle (oldS ^. sAngle))
                     else id
        thrustStrength = 0.06 * fromIntegral dT

        deceleration = ((0.985 ** (fromIntegral dT / 16)) *^)

        steer = (+ if input ^. isHeldA then (-steeringStrength) else 0
                 + if input ^. isHeldD then steeringStrength else 0)
        steeringStrength = 0.0035 * fromIntegral dT


stepAsteroids :: Time -> Asteroids -> Asteroids
stepAsteroids dT = HM.map (stepAsteroid dT)
    where
        stepAsteroid dT a =
            a & aPosition %~ move dT (a ^. aVelocity)


-- | Returns new position
move :: Time -> Velocity -> Position -> Position
move dT (Velocity v) = over pVect addAndWrap
    where
        addAndWrap = wrapPos . (+ fromIntegral dT / 100 *^ v)
        wrapPos (V2 x y) = V2 (wrapAxis x windowWidthF) (wrapAxis y windowHeightF)
        wrapAxis a limit
            | a < (- 40)     = a + limit + 80
            | a > limit + 40 = a - limit - 80
            | otherwise      = a


