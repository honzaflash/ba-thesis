module Draw
( drawScene
) where


import Resources
import Types
import Utility

import qualified SDL
import qualified Data.HashMap.Strict as HM
import Foreign.C.Types ( CInt )
import Linear
import Control.Lens



drawScene :: SDL.Renderer -> Texts -> LoopState -> World -> IO ()
drawScene renderer texts loopState w = do
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
    SDL.clear renderer

    case loopState of
        Playing   -> drawWorld renderer w
        PauseMenu -> drawWorld renderer w >> drawPauseMenu renderer texts
        GameOver  -> drawWorld renderer w >> drawGameOver renderer texts
        MainMenu  -> drawMainMenu renderer texts
        QuitGame  -> pure ()
    
    SDL.present renderer


drawWorld :: SDL.Renderer -> World -> IO ()
drawWorld renderer w = do
    SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
    drawShip renderer $ w ^. wShip
    mapM_ (drawAsteroid renderer) $ w ^. wAsteroids
    mapM_ (drawBullet renderer) $ w ^. wBullets


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


drawMainMenu :: SDL.Renderer -> Texts -> IO ()
drawMainMenu renderer texts =
    drawCenteredText renderer 300 $ texts HM.! TextMainMenu


drawPauseMenu :: SDL.Renderer -> Texts -> IO ()
drawPauseMenu renderer texts =
    drawCenteredText renderer 300 $ texts HM.! TextPaused


drawGameOver :: SDL.Renderer -> Texts -> IO ()
drawGameOver renderer texts =
    drawCenteredText renderer 300 $ texts HM.! TextGameOver


drawCenteredText :: SDL.Renderer -> CInt -> SDL.Texture -> IO ()
drawCenteredText renderer height text = do
    (SDL.TextureInfo _ _ textW textH) <- SDL.queryTexture text
    SDL.copy renderer text Nothing $ Just $
        SDL.Rectangle (SDL.P $ V2 ( (windowWidth - textW) `div` 2 )
                                  height
                      ) (V2 textW textH)

