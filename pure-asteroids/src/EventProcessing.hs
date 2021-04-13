module EventProcessing
    ( processWorldEvents )
    where


import Types

import Linear
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict ((!?))
import Data.Foldable (fold)



-- | Process all WorldEvent
processWorldEvents :: WorldEvents -> World -> World
processWorldEvents events world =
    world
        & wAsteroids %~ processAsteroidsEvents (events ^. forAsteroids)
        & wShip      %~ processShipEvents      (events ^. forShip)
        & wUfos      %~ processUfosEvents      (events ^. forUfos)
        & wBullets   %~ processBulletEvents    (events ^. forBullets)
        & wScore     %~ processScoreEvents     (events ^. forScore)


processAsteroidsEvents :: [AsteroidEvent] -> Asteroids -> Asteroids
processAsteroidsEvents =
    flip $ foldl resolveBreakEvent
    where
        resolveBreakEvent asteroids (BreakE id) =
            case asteroids !? id of
                Nothing -> asteroids
                Just a  -> maybeBreak a id asteroids
        
        maybeBreak a id asteroids
            | minAsteroidSize < a ^. aSize = break a id asteroids
            | otherwise                    = HM.delete id asteroids

        break a id asteroids =
            let
                part1 = a
                    & aVelocity . vVect %~ (\v -> v + perp v)
                    & aSize %~ (`div` 2)
                part2 = a
                    & aId .~ maximum (HM.keys asteroids) + 1
                    & aVelocity . vVect %~ (\v -> v - perp v)
                    & aSize %~ (`div` 2)
            in
                insertAsteroid part2 $ insertAsteroid part1 asteroids

        insertAsteroid a = HM.insert (a ^. aId) a


processShipEvents :: [ShipEvent] -> Ship -> Ship
processShipEvents =
    flip $ foldl processShipEvent
    where
        processShipEvent ship HitE      = ship & sLives -~ 1
        processShipEvent ship GainLifeE = ship & sLives +~ 1


processUfosEvents :: [UfoEvent] -> Ufos -> Ufos
processUfosEvents =
    flip $ foldl const


processBulletEvents :: [BulletEvent] -> Bullets -> Bullets
processBulletEvents =
    flip $ foldl processBulletEvent
    where
        processBulletEvent :: Bullets -> BulletEvent -> Bullets
        processBulletEvent bullets (UfoShootsE (pos, vel)) =
            flip insertBullet bullets $
                Bullet
                { _bId = (+1) $ maximum $ 1 : HM.keys bullets
                , _bPosition = pos
                , _bVelocity = vel
                , _bTtl = initBulletTtl
                , _bShooter = ShotByUfo
                }
        insertBullet b = HM.insert (b ^. bId) b


processScoreEvents :: [ScoreEvent] -> Score -> Score
processScoreEvents =
    flip $ foldr processScoreEvent
    where
        processScoreEvent (IncreaseE x) = sValue +~ x

