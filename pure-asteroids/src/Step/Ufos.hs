module Step.Ufos
( stepUfos
) where


import Types
import Step.Common
import Utility

import Linear
import Control.Lens
import qualified Data.HashMap.Strict as HM



stepUfos :: Time -> RandomStream Double -> World -> Ufos -> (WorldEvents, Ufos)
stepUfos dT rand w =
    fmap (HM.filter ufoLives . spawnUfo rand (w ^. wWaveTime)) .
        traverse (stepUfo dT w)
    where
        ufoLives = (0 <) . view uTtl


-- WouldBeNice: random velocity/direction change (stride)
-- TODO  shooting
stepUfo :: Time -> World -> Ufo -> (WorldEvents, Ufo)
stepUfo dT w ufo = 
    (,) events $
    ufo
      & uPosition %~ move dT (ufo ^. uVelocity)
      & uTtl %~ subtract dT

    where
        events = mempty
                    -- & forBullets   .~ shootEvents TODO
                    & forAsteroids .~ eventsForAsteroids

        shootEvents = [] -- TODO

        eventsForAsteroids = [ BreakE $ a ^. aId |
                                a <- HM.elems $ w ^. wAsteroids,
                                astCollision a ]
        astCollision a = any (isInside a) ufoHitbox
        ufoHitbox =
            let
                (V2 width height) = case ufo ^. uSize of
                                        SmallSaucer -> V2 16 16
                                        LargeSaucer -> V2 32 32
                (V2 left top) = ufo ^. uPosition . pVect - V2 (width/2) (height/2)
            in
                [ V2  left           top
                , V2 (left + width)  top
                , V2  left          (top + height)
                , V2 (left + width) (top + height)
                ]
        -- is a point inside of an asteroid's circle hitbox
        isInside a = (fromIntegral (a ^. aSize) >) . distance (a ^. aPosition . pVect)


spawnUfo :: RandomStream Double -> Time -> Ufos -> Ufos
spawnUfo rand time ufos
    | time > 10000 &&
        head rand < spawnChance =
            insertNewUfo (drop 1 rand) ufos
    | otherwise = ufos

    where
        -- WouldBeNice - better chance / limits on spawning
        spawnChance = 0.1 + 0.00000008 * fromIntegral time

        insertNewUfo rand' = insertUfo newUfo
        insertUfo u = HM.insert (u ^. uId) u
        newUfo =
            Ufo
            { _uId = newUfoId
            , _uPosition = Position $ V2 0 0
            , _uVelocity = Velocity $ V2 6 0
            , _uSize = LargeSaucer
            , _uTtl = 15000
            , _uTimeToShoot = 1000
            }
        newUfoId = (1 +) $ maximum $ 0 : HM.keys ufos
        