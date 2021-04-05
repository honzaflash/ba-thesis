module Step.Ship 
( stepShip
) where

import Types
import Input
import Step.Common
import Utility

import Linear
import qualified Data.HashMap.Strict as HM
import Control.Lens



stepShip :: Time -> InputState -> World -> Ship -> (WorldEvents, Ship)
stepShip dT input w oldS =
    (,) events $
    oldS
        & sPosition         %~ move dT (oldS ^. sVelocity)
        & sVelocity . vVect %~ thrust . deceleration
        & sAngle            %~ steer
        & sLives            %~ if nullEvents events then id else subtract 1

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
