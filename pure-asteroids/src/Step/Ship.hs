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
        ShipExploding _  -> stepExplodingShip dT s
        ShipRespawning _ -> stepRespawningShip dT input s
        

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
            [ BreakE $ a ^. aId
                | a <- HM.elems $ w ^. wAsteroids
                , astCollision a
            ]
        astCollision a = any (isInsideAst a) $ take 3 $ shipPoints oldS -- 4th doesn't need to be checked
        isInsideAst a = (fromIntegral (a ^. aSize) >) . distance (a ^. aPosition . pVect)

        -- ship colliding with ufo
        eventsForUfos =
            [ DestroyE $ u ^. uId
                | u <- HM.elems $ w ^. wUfos
                , ufoCollision u
            ]
        ufoCollision u = any (isInsideUfo u) $ take 3 $ shipPoints oldS
        isInsideUfo u point =
            -- just an elliptical hit box
            let uPos = u ^. uPosition . pVect
                eccentricity = V2 (uSizeMult * 18) 0
                focus1 = uPos - eccentricity
                focus2 = uPos + eccentricity
                mjAxis = uSizeMult * 40
                uSizeMult = case u ^. uSize of
                                SmallSaucer -> 1
                                LargeSaucer -> 2
            in
                distance point focus1 + distance point focus2 < mjAxis


-- | step function for ship in the exploding state
stepExplodingShip :: Time -> Ship -> (WorldEvents, Ship)
stepExplodingShip dT ship =
    (,) mempty $
    ship
      & sAngle +~ 0.01 * fromIntegral dT -- just spin
      & sState %~ stepShipState dT
      & resetIfTransitioningState

    where
        resetIfTransitioningState ship =
            case ship ^. sState of
                ShipExploding t
                    | t <= 0    -> resetShip ship
                _               -> ship
        resetShip ship =
            ship
              & sPosition . pVect .~ V2 (windowWidthF / 2) (windowHeightF / 2)
              & sVelocity . vVect .~ V2 0 0
              & sAngle            .~ pi / 2 * 3


-- | step function for ship in the respawning state
stepRespawningShip :: Time -> InputState -> Ship -> (WorldEvents, Ship)
stepRespawningShip dT input ship =
    (,) mempty $
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

