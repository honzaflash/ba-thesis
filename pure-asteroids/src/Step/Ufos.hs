module Step.Ufos
( stepUfos
, predictiveShooting
) where


import Types
import Step.Common
import Utility

import qualified Data.HashMap.Strict as HM
import Linear
import Control.Lens



stepUfos :: Time -> RandomStream Double -> World -> Ufos -> (WorldEvents, Ufos)
stepUfos dT rand w =
    fmap (spawnUfo rand (w ^. wWaveTime)) .
        traverse (stepUfo dT w) . filterOutDeadUfos
    where
        filterOutDeadUfos = HM.filter $ (0 <) . view uTtl


-- WouldBeNice: random velocity/direction change (stride)
stepUfo :: Time -> World -> Ufo -> (WorldEvents, Ufo)
stepUfo dT w oldU = 
    (,) events $
    oldU
      & uPosition %~ move dT (oldU ^. uVelocity)
      & uTimeToShoot %~ (if null shootEvents then subtract dT else const 2000)
      & uTtl %~ if null eventsForAsteroids then subtract dT else const 0

    where
        events = mempty
                    & forBullets   .~ shootEvents
                    & forAsteroids .~ eventsForAsteroids
        
        -- asteroid x ufo collision events
        eventsForAsteroids = [ BreakE $ a ^. aId |
                                a <- HM.elems $ w ^. wAsteroids,
                                astCollision a ]
        astCollision a = any (isInside a) $ ufoPoints oldU
        -- returns whether a point is inside of an asteroid's circle hitbox
        isInside a = (fromIntegral (a ^. aSize) >) . distance (a ^. aPosition . pVect)

        -- generates an event for when a ufo shoots
        shootEvents =
            [ UfoShootsE (oldU ^. uPosition) bulletVelocity
                | oldU ^. uTimeToShoot <= 0 ]
        bulletVelocity =
            Velocity $ (ufoBulletSpeed *^) $ angle $
                case oldU ^. uSize of
                    SmallSaucer -> predictiveShooting oldU (w ^. wShip)
                    LargeSaucer -> simpleShooting     oldU (w ^. wShip)


-- | Predicts ship's position based on its current velocity
--   and returns an angle for the bullet's velocity
predictiveShooting :: Ufo -> Ship -> Double
predictiveShooting u s =
    unangle (sPos - uPos) + asin (norm sVel * sin beta / ufoBulletSpeed)
                   -- law of sines
    where
        beta = unangle (uPos - sPos) - unangle sVel
        sVel = s ^. sVelocity . vVect
        sPos = s ^. sPosition . pVect
        uPos = u ^. uPosition . pVect


-- | Returns an angle for the bullet's velocity
--   based on the ship's current position
simpleShooting :: Ufo -> Ship -> Double
simpleShooting u s =
    snapToEights $ unangle (sPos - uPos)
    where
        snapToEights alfa =
            fromIntegral (round (alfa / 2 / pi * 8) :: Int) / 8 * 2 * pi
        sPos = s ^. sPosition . pVect
        uPos = u ^. uPosition . pVect


-- | randomly and increasingly spawn ufos
spawnUfo :: RandomStream Double -> Time -> Ufos -> Ufos
spawnUfo rand time ufos
    | time > 15000 &&
        rand !! 1 < spawnChance =
            insertNewUfo ufos
    | otherwise = ufos

    where
        -- WouldBeNice - better chance / limits on spawning
        spawnChance = min 1 $ 0.1 + 0.0000005 * fromIntegral time

        insertNewUfo = insertUfo newUfo
        insertUfo u = HM.insert (u ^. uId) u
        newUfo =
            Ufo
            { _uId = newUfoId
            , _uPosition = Position $ V2 0 startY
            , _uVelocity = Velocity $ V2 6 0
            , _uSize = if rand !! 2 < sizeChance then SmallSaucer else LargeSaucer
            , _uTtl = 16000
            , _uTimeToShoot = 1500
            }
        newUfoId = (1 +) $ maximum $ 0 : HM.keys ufos
        startY = (rand !! 3) / 100 * windowHeightF
        sizeChance = max 20 $ min 80 $ 0.001 * fromIntegral time

