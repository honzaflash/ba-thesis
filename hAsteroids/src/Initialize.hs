module Initialize ( initializeWorld ) where

import Components

import Apecs
import Control.Monad ( void )
import Linear ( V2(V2) )

initializeWorld :: IO World
initializeWorld = do
    world <- initWorld
    runWith world $ void $ do
        newEntity (Ship 0, Position $ V2 0 0, Velocity $ V2 0 0)
        newEntity (Asteroid 100, Position $ V2 100 400, Velocity $ V2 5 3)
    return world

