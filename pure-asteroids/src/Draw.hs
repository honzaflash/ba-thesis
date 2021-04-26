module Draw
( drawScene
) where


import Resources
import Types
import Utility

import qualified SDL
import qualified Data.HashMap.Strict as HM
import Foreign.C.Types ( CInt )
import Data.Foldable ( fold )
import Control.Monad ( zipWithM_ )
import Control.Lens
import Linear



drawScene :: SDL.Renderer -> Texts -> LoopState -> World -> IO ()
drawScene renderer texts loopState w = do
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
    SDL.clear renderer

    case loopState of
        Playing   -> drawWorld renderer texts w
        PauseMenu -> drawWorld renderer texts w >> drawPauseMenu renderer texts
        GameOver  -> drawWorld renderer texts w >> drawGameOver renderer texts
        MainMenu  -> drawMainMenu renderer texts
        QuitGame  -> pure ()
    
    SDL.present renderer


drawWorld :: SDL.Renderer -> Texts -> World -> IO ()
drawWorld renderer texts w = do
    SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
    drawShip renderer $ w ^. wShip
    drawLives renderer texts $ w ^. wShip . sLives
    mapM_ (drawAsteroid renderer) $ w ^. wAsteroids
    mapM_ (drawBullet renderer) $ w ^. wBullets
    mapM_ (drawUfo renderer) $ w ^. wUfos
    drawScore renderer texts $ w ^. wScore


drawShip :: SDL.Renderer -> Ship -> IO ()
drawShip renderer =
    drawShape renderer . shipPoints


drawAsteroid :: SDL.Renderer -> Asteroid -> IO ()
drawAsteroid renderer a =
    drawShape renderer asteroidShape
    where
        -- generates regular heptagon of given size
        asteroidShape =
            map ((+ a ^. aPosition . pVect) . asteroidVertex) [1..7]
        asteroidVertex i = V2 ((*) sizeF . cos $ pi * 2 / 7 * i)
                                ((*) sizeF . sin $ pi * 2 / 7 * i)
        sizeF = fromIntegral $ a ^. aSize


drawUfo :: SDL.Renderer -> Ufo -> IO ()
drawUfo renderer=
    drawShape renderer . ufoPoints


drawBullet :: SDL.Renderer -> Bullet -> IO ()
drawBullet renderer =
    SDL.drawPoint renderer . SDL.P . fmap round . view (bPosition . pVect)


-- | Helper function for drawing closed shapes
drawShape :: RealFrac a => SDL.Renderer -> [V2 a] -> IO ()
drawShape r = (\pts -> drawLines pts $ head pts) . map (SDL.P . fmap round)
    -- is this slower than SDL.drawLines??
    where
        drawLines [  ]       _      = pure ()
        drawLines [pt]       termPt = SDL.drawLine r pt termPt
        drawLines (p1 : p2 : pts) termPt =
            SDL.drawLine r p1 p2 >> drawLines (p2 : pts) termPt


drawScore :: SDL.Renderer -> Texts -> Score -> IO ()
drawScore renderer texts =
    drawNumber renderer texts (V2 (windowWidth `div` 2) 20) . _sValue


drawLives :: SDL.Renderer -> Texts -> Int -> IO ()
drawLives renderer texts =
    drawNumber renderer texts (V2 20 20)


drawMainMenu :: SDL.Renderer -> Texts -> IO ()
drawMainMenu renderer texts =
    drawCenteredTexts renderer texts
        [ TextMainMenu
        , TextPressSpaceToStartNewGame
        , TextPressEscapeToQuit
        ]


drawPauseMenu :: SDL.Renderer -> Texts -> IO ()
drawPauseMenu renderer texts =
    drawCenteredTexts renderer texts
        [ TextPaused
        , TextPressSpaceToUnpause
        , TextPressEscapeToExitToMainMenu
        ]


drawGameOver :: SDL.Renderer -> Texts -> IO ()
drawGameOver renderer texts =
    drawCenteredTexts renderer texts
        [TextGameOver
        , TextPressSpaceToContinue
        ]


drawNumber ::
    ( Show a, Num a )
    => SDL.Renderer
    -> Texts
    -> V2 CInt
    -> a
    -> IO ()
drawNumber renderer texts pos =
    zipWithM_ (drawText renderer texts)
        [pos + V2 (0 + i * 16) 0 | i <- [0..]] . numToTextKeys


drawCenteredTexts :: SDL.Renderer -> Texts -> [TextKey] -> IO ()
drawCenteredTexts renderer texts =
    zipWithM_ (drawCenteredText renderer texts) [100 * i | i <- [2..]]


drawCenteredText :: SDL.Renderer -> Texts -> CInt -> TextKey -> IO ()
drawCenteredText renderer texts yPos key = do
    textW <- SDL.textureWidth <$> SDL.queryTexture (texts HM.! key)
    drawText renderer texts (V2 ((windowWidth - textW) `div` 2 ) yPos) key


drawText :: SDL.Renderer -> Texts -> V2 CInt -> TextKey -> IO ()
drawText renderer texts pos key = do
    let text = texts HM.! key
    (SDL.TextureInfo _ _ textW textH) <- SDL.queryTexture text
    SDL.copy renderer text Nothing $ Just $
        SDL.Rectangle (SDL.P pos) (V2 textW textH)

