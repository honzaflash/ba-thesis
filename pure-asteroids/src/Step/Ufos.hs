module Step.Ufos
( stepUfos
) where


import Types
import Step.Common

import Control.Lens



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

