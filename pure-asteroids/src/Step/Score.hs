module Step.Score
( stepScore
) where


import Types

import Control.Lens



stepScore :: Time -> Score -> (WorldEvents, Score)
stepScore dT score =
    (,) events $
    score
      & sLivesAwarded +~ if nullEvents events then 0 else 1
    
    where
        events =
            mempty
              & forShip %~ if tenThousandReached then (GainLifeE :) else id
        
        tenThousandReached =
            score ^. sValue > (1 + score ^. sLivesAwarded) * 10000
