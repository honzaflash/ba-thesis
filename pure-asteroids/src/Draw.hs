module Draw where

import Types

import qualified SDL
import qualified Data.HashMap.Strict as HM
import Linear


drawWorld :: SDL.Renderer -> World -> IO ()
drawWorld renderer w = do
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
    mapM_ (drawAsteroid renderer) $ HM.elems $ wAsteroids w
    drawShip renderer $ wShip w

    SDL.present renderer


drawShip :: SDL.Renderer -> Ship -> IO ()
drawShip renderer s =
    let sPos = fmap round $ unposition $ shipPosition s
        p1 = SDL.P sPos
        p2 = SDL.P $ sPos + fmap round (10 *^ angle (shipAngle s))
    in SDL.drawLine renderer p1 p2

drawAsteroid :: SDL.Renderer -> Asteroid -> IO ()
drawAsteroid renderer a =
    SDL.drawPoint renderer $ SDL.P $ fmap round $ unposition $ astPosition a
