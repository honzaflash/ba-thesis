module Draw where


import Types
import Utility

import qualified SDL
import qualified Data.HashMap.Strict as HM
import Linear
import Control.Lens



drawWorld :: SDL.Renderer -> World -> IO ()
drawWorld renderer w = do
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
    mapM_ (drawAsteroid renderer) $ w ^. wAsteroids
    drawShip renderer $ w ^. wShip

    SDL.present renderer


drawShip :: SDL.Renderer -> Ship -> IO ()
drawShip renderer ship = drawShape renderer $ shipPoints ship


drawAsteroid :: SDL.Renderer -> Asteroid -> IO ()
drawAsteroid renderer a =
    drawShape renderer asteroidShape
    where
        asteroidShape =
            map ((+ a ^. aPosition . pVect) . asteroidVertex) [1..7]
        asteroidVertex i = V2 ((*) sizeF . cos $ pi * 2 / 7 * i)
                                ((*) sizeF . sin $ pi * 2 / 7 * i)
        sizeF = fromIntegral $ a ^. aSize