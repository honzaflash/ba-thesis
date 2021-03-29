module Draw where


import Types

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
drawShip renderer s =
    let sPos = fmap round $ s ^. sPosition . pVect
        p1 = SDL.P sPos
        p2 = SDL.P $ sPos + fmap round (10 *^ angle (s ^. sAngle))
    in SDL.drawLine renderer p1 p2


drawAsteroid :: SDL.Renderer -> Asteroid -> IO ()
drawAsteroid renderer a =
    SDL.drawPoint renderer $ SDL.P $ fmap round $ a ^. aPosition . pVect
