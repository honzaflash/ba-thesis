module Step.Ufos
( stepUfos
) where


import Types
import Step.Common

import Linear
import Control.Lens
import qualified Data.HashMap.Strict as HM



stepUfos :: Time -> World -> Ufos -> (WorldEvents, Ufos)
stepUfos dT w = traverse (stepUfo dT w)


-- TODO  random velocity/direction change (stride)
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
        -- is a point inside of an asteroid circle hitbox
        isInside a = (fromIntegral (a ^. aSize) >) . distance (a ^. aPosition . pVect)


