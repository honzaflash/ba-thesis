module Draw where


import Components
import Resources
import SdlWrappers
import Utility ( windowWidth )

import Apecs
import qualified SDL
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Internal.Strict ( (!) )
import Foreign.C.Types ( CDouble, CInt )
import Control.Monad ( zipWithM_ )
import Linear



-- | Takes care of all the rendering
drawScene :: SystemWithResources ()
drawScene = do 
    lift clearRenderer

    loopState <- get global
    case loopState of
        Playing -> drawWorld
        Paused -> drawWorld >> drawPaused
        GameOver -> drawWorld >> drawGameOver
        InMenu -> drawInMenu
        _ -> pure ()
    
    lift presentRenderer

    where
        drawInMenu = lift $
            drawCenteredTexts
                [ TextMainMenu
                , TextPressSpaceToStartNewGame
                , TextPressEscapeToQuit
                ]

        drawPaused = lift $
            drawCenteredTexts
                [ TextPaused
                , TextPressSpaceToUnpause
                , TextPressEscapeToExitToMainMenu
                ]

        drawGameOver = lift $
            drawCenteredTexts
                [ TextGameOver
                , TextPressSpaceToContinue
                ]


-- | Draw the background, entities and UI
drawWorld :: SystemWithResources ()
drawWorld = do
    lift $ drawBackground
    drawAsteroids
    drawBullets
    drawUfos
    drawShip
    drawLivesAndScore


drawShip :: SystemWithResources ()
drawShip =
    cmapM_ $ \(Ship a, Position pos) -> lift $
        copyExWR ShipTexture pos (V2 60 40) $ a / pi * 180


drawUfos :: SystemWithResources ()
drawUfos =
    cmapM_ $ \(Ufo _ ufoSize, Position pos) -> lift $
        copyWR (ufoSizeToKey ufoSize) pos $
            V2 40 20 ^* case ufoSize of
                            SmallSaucer -> 1
                            LargeSaucer -> 2
    where
        ufoSizeToKey LargeSaucer = LargeSaucerTexture
        ufoSizeToKey SmallSaucer = SmallSaucerTexture


drawBullets :: SystemWithResources ()
drawBullets =
    cmapM_ $ \(Bullet _, Position pos, Velocity vel) -> lift $
        copyExWR BulletTexture pos (V2 25 10) $ unangle vel / pi * 180


drawAsteroids :: SystemWithResources ()
drawAsteroids =
    cmapM_ $ \(Asteroid size, Position pos) -> lift $
        let textureKey = case size of
                            32  -> SmallAsteroidTexture
                            64  -> MediumAsteroidTexture
                            128 -> LargeAsteroidTexture
        in copyWR textureKey pos $ pure size


drawLivesAndScore :: SystemWithResources ()
drawLivesAndScore = do
    (ShipLives lives, Score score) <- get global
    lift $ drawNumber (V2 20 20) lives
    lift $ drawNumber (V2 (windowWidth `div` 2) 20) score


drawBackground :: WithResources ()
drawBackground = copyWRMaybeRect BackgroundTexture Nothing


drawNumber :: ( Show a, Num a ) => V2 CInt -> a -> WithResources ()
drawNumber pos =
    zipWithM_ drawText
        [pos + V2 (0 + i * 16) 0 | i <- [0..]] . numToTextKeys


drawCenteredTexts :: [TextKey] -> WithResources ()
drawCenteredTexts =
    zipWithM_ drawCenteredText [100 * i | i <- [2..]]


drawCenteredText :: CInt -> TextKey -> WithResources ()
drawCenteredText yPos key = do
    texts <- asks resTexts
    textW <- SDL.textureWidth <$> SDL.queryTexture (texts HM.! key)
    drawText (V2 ((windowWidth - textW) `div` 2 ) yPos) key


drawText :: V2 CInt -> TextKey -> WithResources ()
drawText pos key = do
    renderer <- asks resRenderer
    texts <- asks resTexts
    let text = texts HM.! key
    (SDL.TextureInfo _ _ textW textH) <- SDL.queryTexture text
    SDL.copy renderer text Nothing $ Just $
        SDL.Rectangle (SDL.P pos) (V2 textW textH)

