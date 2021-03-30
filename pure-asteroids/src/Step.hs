module Step where


import Types
import Utility ( windowWidthF, windowHeightF, shipPoints )

import Linear
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Data.Foldable ( fold )



-- TODO  add keyboard events machinery -> different module
stepWorld :: Time -> World -> (WorldEvents, World)
stepWorld deltaTime oldW =
    let
        (eventsS, newShip) = stepShip deltaTime oldW $ oldW ^. wShip
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


stepShip :: Time -> World -> Ship -> (WorldEvents, Ship)
stepShip dT w oldS =
    (,) events $
    oldS
        & sPosition .~ move dT (oldS ^. sVelocity) (oldS ^. sPosition)
        & sVelocity .~ Velocity (V2 1 1) -- TODO thrust function
        & sAngle    .~ oldS ^. sAngle + 0.01 -- TODO steer function
        & sLives    %~ case events of
                        (WorldEvents [] [] [] []) -> id
                        _                         -> subtract 1

    where
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

-- collision :: (V2 Double, V2 Double) -> (V2 Double, V2 Double) -> Bool
-- collision (position1, size1) (position2, size2) = False -- todo

