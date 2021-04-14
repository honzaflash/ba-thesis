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
stepShip dT input w s =
    case s ^. sState of
        ShipAlive        -> stepPlayingShip dT input w s
        ShipExploding t  -> stepExplodingShip dT s
        ShipRespawning t -> stepRespawningShip dT input s
        

stepPlayingShip :: Time -> InputState -> World -> Ship -> (WorldEvents, Ship)
stepPlayingShip dT input w oldS =
    (,) events $
    oldS
      & sPosition         %~ move dT (oldS ^. sVelocity)
      & sVelocity . vVect %~ thrust dT input (oldS ^. sAngle) . decelerate dT
      & sAngle            +~ steering dT input
      & sLives            %~ (if nullEvents events then id else subtract 1)
      -- if lives are 0, state will transition next frame:
      & sState            %~ if nullEvents events then id else const $ ShipExploding 500

    where
        -- Event generation
        events = mempty
                    & forAsteroids .~ eventsForAsteroids
                    & forUfos      .~ eventsForUfos

        eventsForAsteroids =
            [ BreakE $ a ^. aId |
                a <- HM.elems $ w ^. wAsteroids,
                astCollision a ]
        astCollision a = any (isInside a) $ take 3 $ shipPoints oldS -- 4th doesn't need to be checked
        isInside a = (fromIntegral (a ^. aSize) >) . distance (a ^. aPosition . pVect)

        eventsForUfos = []


-- | step function for ship in the exploding state
stepExplodingShip :: Time -> Ship -> (WorldEvents, Ship)
stepExplodingShip dT ship = (,) mempty $
    ship
      & sAngle +~ 0.01 * fromIntegral dT -- just spin
      & sState %~ stepShipState dT
      & resetIfTransitioningState

    where
        resetIfTransitioningState ship =
            case ship ^. sState of
                ShipExploding t
                    | t < 0 -> resetShip ship
                _           -> ship
        resetShip ship =
            ship
              & sPosition . pVect .~ V2 (windowWidthF / 2) (windowHeightF / 2)
              & sVelocity . vVect .~ V2 0 0
              & sAngle            .~ 0


-- | step function for ship in the respawning state
stepRespawningShip :: Time -> InputState -> Ship -> (WorldEvents, Ship)
stepRespawningShip dT input ship = (,) mempty $
    ship
      & sPosition         %~ move dT (ship ^. sVelocity)
      & sVelocity . vVect %~ thrust dT input (ship ^. sAngle) . decelerate dT
      & sAngle            +~ steering dT input
      & sState %~ stepShipState dT


-- * Control functions

-- | Returns a function to modify velocity vector based on input
thrust :: Time -> InputState -> Angle -> V2 Double -> V2 Double
thrust dT input direction = if input ^. isHeldW 
                               then (+ thrustStrength *^ angle direction)
                               else id
    where thrustStrength = 0.04 * fromIntegral dT


-- | Constant deceletration over time
decelerate :: Time -> V2 Double -> V2 Double
decelerate dT = ((0.985 ** (fromIntegral dT / 16)) *^)


-- | returns delta angle that should be added to the current
--   ship direction, based on the input
steering :: Time -> InputState -> Angle
steering dT input = (if input ^. isHeldA then (-steeringStrength) else 0)
                    + if input ^. isHeldD then steeringStrength else 0
    where steeringStrength = 0.0025 * fromIntegral dT


-- | State decrementor
stepShipState :: Time -> ShipState -> ShipState
stepShipState dT (ShipExploding t)
                    | t > 0     = ShipExploding $ t - dT
                    | otherwise = ShipRespawning 1000
stepShipState dT (ShipRespawning t)
                    | t > 0     = ShipRespawning $ t - dT
                    | otherwise = ShipAlive
stepShipState _   ShipAlive     = ShipAlive -- shouldn't be necessary

