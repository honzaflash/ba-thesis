{-# LANGUAGE TypeApplications           #-}
module Collisions
( detectAndHandleCollisions
) where


import Components
import Resources
import Utility

import Apecs
import Linear
import Foreign.C.Types ( CInt, CDouble )
import Control.Monad ( when )



-- | Detect and handle all the collisions in the world
detectAndHandleCollisions :: SystemWithResources ()
detectAndHandleCollisions = do
    -- detect and handle collisions between individual groups
    asteroidsVsTheRest
    ufosVsShipAndBullets
    shipVsBullets


-- | Detect and handle collision between asteroids and the rest
asteroidsVsTheRest :: SystemWithResources ()
asteroidsVsTheRest =
    cmapM_ $ \(Asteroid aSize, Position aPos, aEty) -> do

        -- asteroid x ship
        cmapM_ $ \(Ship alfa, Position sPos, sState) ->
            let
                collision =
                    any (isPointInCircle aPos aSize) $
                        shipTriangle alfa sPos
            in 
                when (sState == Alive && collision) $ do
                    shipIsHit
                    asteroidIsHit aSize ShotByShip aEty

        -- asteroid x ufo
        cmapM_ $ \(Ufo _ uSize, Position uPos, uEty) ->
            let
                collision =
                    any (isPointInCircle aPos aSize) $
                        ufoRectangle uSize uPos
            in
                when collision $ do
                    ufoIsHit uSize uEty
                    asteroidIsHit aSize ShotByUfo aEty
        
        -- asteroid x bullet
        cmapM_ $ \(Bullet shooter, Position bPos, bEty) ->
            let
                collision = isPointInCircle aPos aSize bPos
            in
                when collision $ do
                    bulletIsHit bEty
                    asteroidIsHit aSize shooter aEty
    
    where
        isPointInCircle center d pt =
            distance pt center < fromIntegral d / 2


-- | Detect and handle collision between ufos and the ship or bullets
ufosVsShipAndBullets :: SystemWithResources ()
ufosVsShipAndBullets =
    cmapM_ $ \(Ufo _ uSize, Position uPos, uEty) -> do

        -- ufo x ship
        cmapM_ $ \(Ship alfa, Position sPos) ->
            let
                collision = any (isPointInEllipse uPos uSize) $
                    shipTriangle alfa sPos
            in 
                when collision $ do
                    shipIsHit
                    ufoIsHit uSize uEty
        
        -- ufo x bullet
        cmapM_ $ \(Bullet shooter, Position bPos, bEty) ->
            let
                collision = isPointInEllipse uPos uSize bPos
            in 
                when (shooter == ShotByShip && collision) $ do
                    bulletIsHit bEty
                    ufoIsHit uSize uEty
    where
        isPointInEllipse uPos uSize pt =
            let 
                eccentricity = V2 (uSizeMult * 18) 0
                focus1 = uPos - eccentricity
                focus2 = uPos + eccentricity
                mjAxis = uSizeMult * 40
                uSizeMult = case uSize of
                                SmallSaucer -> 1
                                LargeSaucer -> 2
            in 
                distance pt focus1 + distance pt focus2 < mjAxis


-- | Detect and handle collisions between the ship and bullets
shipVsBullets :: SystemWithResources ()
shipVsBullets =
    cmapM_ $ \(Ship _, Position sPos, sState) ->

        -- ship x bullet
        cmapM_ $ \(Bullet shooter, Position bPos, bEty) ->
            let
                -- using square hit box (almost like the original Atari)
                -- might be worth an upgrade though
                collision =
                    shooter == ShotByUfo &&
                        left < bx && bx < left + width &&
                        top  < by && by < top  + height
                (V2 bx by)        = bPos
                (V2 width height) = V2 36 36
                (V2 left top)     = sPos - V2 18 18
            in 
                when (sState == Alive && collision) $ do
                    shipIsHit
                    bulletIsHit bEty


-- | Handle asteroid collision
asteroidIsHit :: CInt -> ShotBy -> Entity -> SystemWithResources ()
asteroidIsHit aSize destroyer aEty = do
    when (destroyer == ShotByShip) $
        modify global $ \(Score s) -> Score $ s + reward
    breakAsteroid

    where
        reward
            | aSize == initAsteroidSize                 = 20
            | aSize == initAsteroidSize `div` 2         = 50
            | aSize == initAsteroidSize `div` 2 `div` 2 = 100

        breakAsteroid
            | aSize == initAsteroidSize `div` 2 `div` 2 =
                destroy aEty $ Proxy @AsteroidComponents
            | otherwise = do
                (Position pos, Velocity vel) <- get aEty
                newEntity (Asteroid $ aSize `div` 2, Velocity $ vel + perp vel, Position pos)
                set aEty (Asteroid $ aSize `div` 2, Velocity $ vel - perp vel)


-- | Handle ship collision
shipIsHit :: SystemWithResources ()
shipIsHit =
    modify global $ \(ShipLives x) ->
        ( ShipLives $ x - 1
        , Exploding 700
        , if x == 1 then GameOver else Playing
        )
    -- set sEty (Velocity $ V2 0 0) -- stop ship


-- | Handle ufo collision
ufoIsHit :: UfoSize -> Entity -> SystemWithResources ()
ufoIsHit uSize uEty = do
    destroy uEty $ Proxy @UfoComponents
    modify global $ \(Score s) -> Score $ s + reward
    where
        reward = case uSize of
                     SmallSaucer -> 1000
                     LargeSaucer -> 200


-- | Handle bullet collision
bulletIsHit :: Entity -> SystemWithResources ()
bulletIsHit bEty =
    destroy bEty $ Proxy @BulletComponents


-- | calculates the ship vertices for simple collision detection
shipTriangle :: Angle -> V2 CDouble -> [V2 CDouble]
shipTriangle a sPos =
    map (+ sPos)
        [ -28 *^ facing - 18 *^ perp facing -- left
        ,  28 *^ facing                     -- front tip
        , -28 *^ facing + 18 *^ perp facing -- right
        ]
        -- slightly smaller than the drawn size
        -- which is a 40 x 60 texture
    where facing = angle a


-- | calculates the ufo vertices for simple collision detection
ufoRectangle :: UfoSize -> V2 CDouble -> [V2 CDouble]
ufoRectangle uSize uPos =
    map (+ uPos)
        [ -2 * unitX - unitY
        ,  2 * unitX - unitY
        , -2 * unitX + unitY
        ,  2 * unitX + unitY
        ]
        -- slightly smaller than the drawing box
    where
        unitX = V2 unit 0
        unitY = V2 0    unit
        unit = case uSize of
                   SmallSaucer -> 9
                   LargeSaucer -> 18

