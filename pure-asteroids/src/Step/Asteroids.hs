module Step.Asteroids where


import Types
import Step.Common

import qualified Data.HashMap.Strict as HM 
import Control.Lens



stepAsteroids :: Time -> Asteroids -> Asteroids
stepAsteroids dT = HM.map (stepAsteroid dT)
    where
        stepAsteroid dT a =
            a & aPosition %~ move dT (a ^. aVelocity)

