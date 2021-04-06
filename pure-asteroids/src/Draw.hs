module Draw
( drawWorld
, drawMainMenu
, drawPauseMenu
, drawGameOver
) where


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
    drawShip renderer $ w ^. wShip
    mapM_ (drawAsteroid renderer) $ w ^. wAsteroids
    mapM_ (drawBullet renderer) $ w ^. wBullets

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


drawBullet :: SDL.Renderer -> Bullet -> IO ()
drawBullet renderer b =
    SDL.drawPoint renderer $ SDL.P $ fmap round $ b ^. bPosition . pVect


drawMainMenu :: SDL.Renderer -> IO ()
drawMainMenu _ = putStrLn "press SPACE to start a new game\npress ESCAPE to quit the game"


drawPauseMenu :: SDL.Renderer -> IO ()
drawPauseMenu _ = putStrLn "press SPACE to unpause\npress ESCAPE to exit to main menu"


drawGameOver :: SDL.Renderer -> IO ()
drawGameOver _ = putStrLn "GAME OVER\npress SPACE to continue to main menu"

