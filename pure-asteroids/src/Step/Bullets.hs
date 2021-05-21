module Step.Bullets 
( stepBullets
) where

import Step.Common
import Types
import Input

import Linear
import Control.Lens
import qualified Data.HashMap.Strict as HM



-- | Step function for the entire Bullets collection
stepBullets :: Time -> InputState -> World -> Bullets -> (WorldEvents, Bullets)
stepBullets dT input w =
    fmap (shoot input w) . traverse (stepBullet dT w) . filterDeadBullets
    where
        filterDeadBullets = HM.filter (\b -> b ^. bTtl > 0)


-- | Step function for an individual Bullet
stepBullet :: Time -> World -> Bullet -> (WorldEvents, Bullet)
stepBullet dT w oldB =
    (,) events $
    oldB
       & bPosition %~ move dT (oldB ^. bVelocity)
       -- mark bullet for death if it collided with anything
       & bTtl %~ if nullEvents events then subtract dT else const 0
    
    where
        events = asteroidCollisionEvents 
                 <> shipCollisionEvents
                 <> ufosCollisionEvents 
        
        -- Collisions with ASTEROIDS
        asteroidCollisionEvents =
            foldl genEventsAst mempty $ HM.elems $ w ^. wAsteroids

        genEventsAst events' a
            | astCollision a =
                    events'
                        & forAsteroids %~ (BreakE (a ^. aId) :)
                        & forScore     %~ if oldB ^. bShooter == ShotByShip
                                              then (IncreaseE (astReward a) :)
                                              else id
            | otherwise = events'

        astCollision a = isInside a $ oldB ^. bPosition . pVect 
        isInside a = (fromIntegral (a ^. aSize) >) . distance (a ^. aPosition . pVect)
        
        astReward a
            | a ^. aSize == initAsteroidSize                 = 20
            | a ^. aSize == initAsteroidSize `div` 2         = 50
            | a ^. aSize == initAsteroidSize `div` 2 `div` 2 = 100
            -- otherwise = boom! something is very wrong

        -- Collision with SHIP
        shipCollisionEvents =
            mempty & forShip %~ if shipColliding then (HitE :) else id

        shipColliding =
            -- using square hit box (almost like the original Atari)
            -- might be worth an upgrade though
            let (V2 bx by) = oldB ^. bPosition . pVect
                (V2 width height) = V2 30 30
                (V2 left top) = w ^. wShip . sPosition . pVect - V2 15 15
            in
                oldB ^. bShooter == ShotByUfo
                && (left < bx && bx < left + width && top < by && by < top + height)

        -- Collisions with UFOS
        ufosCollisionEvents =
            foldl genEventsUfos mempty $ HM.elems $ w ^. wUfos
        
        genEventsUfos events' u
            | ufoCollision u =
                    events'
                        & forUfos  %~ (DestroyE (u ^. uId) :)
                        & forScore %~ if oldB ^. bShooter == ShotByShip
                                              then (IncreaseE (ufoReward u) :)
                                              else id
            | otherwise = events'
        
        ufoCollision u =
            -- just an elliptical hit box
            let bPos = oldB ^. bPosition . pVect
                uPos = u ^. uPosition . pVect
                eccentricity = V2 (uSizeMult * 18) 0
                focus1 = uPos - eccentricity
                focus2 = uPos + eccentricity
                mjAxis = uSizeMult * 40
                uSizeMult = case u ^. uSize of
                                SmallSaucer -> 1
                                LargeSaucer -> 2
            in
                oldB ^. bShooter == ShotByShip
                && distance bPos focus1 + distance bPos focus2 < mjAxis
        
        ufoReward u = case u ^. uSize of
                          SmallSaucer -> 1000
                          LargeSaucer -> 200


shoot :: InputState -> World -> Bullets -> Bullets
shoot input w =
    if wasPressed input spaceKeycode
        then insertBullet
        else id

    where
        insertBullet bullets = 
            let newId = maximum (1 : HM.keys bullets) + 1
            in  HM.insert newId (newBullet newId) bullets

        newBullet newId =
            Bullet newId newBulletPosition newBulletVelocity ShotByShip initBulletTtl
        newBulletPosition = w ^. wShip . sPosition
        newBulletVelocity = Velocity $ bulletSpeed *^ angle (w ^. wShip . sAngle)

