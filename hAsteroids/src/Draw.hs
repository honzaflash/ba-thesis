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


drawWorld :: SystemWithResources ()
drawWorld = do
    drawBackground
    drawShip
    drawBullets
    drawAsteroids


drawShip :: SystemWithResources ()
drawShip =
    cmapM_ $ \(Ship a, Position pos) -> lift $
        copyExWR "Ship" pos (V2 60 30) $ a / pi * 180
        

drawBullets :: SystemWithResources ()
drawBullets =
    cmapM_ $ \(Bullet _, Position pos, Velocity vel) -> lift $
        copyExWR "Bullet" pos (V2 25 10) $ unangle vel / pi * 180


drawAsteroids :: SystemWithResources ()
drawAsteroids =
    cmapM_ $ \(Asteroid size, Position pos) -> lift $
        copyWR "Small" pos $ pure size


drawBackground :: SystemWithResources ()
drawBackground = lift $ copyWRMaybeRect "Background" Nothing


-- drawMenu :: SystemWithResources ()
-- drawMenu = do
--     renderer <- askForRenderer
--     texts <- askForTexts

--     drawText renderer texts 0 TextPaused
    -- drawText renderer texts 100 TextPressEscape


drawInMenu :: SystemWithResources ()
drawInMenu =
    drawCenteredTexts
        [ TextMainMenu
        , TextPressSpaceToStartNewGame
        , TextPressEscapeToQuit
        ]


drawPaused :: SystemWithResources ()
drawPaused =
    drawCenteredTexts
        [ TextPaused
        , TextPressSpaceToUnpause
        , TextPressEscapeToExitToMainMenu
        ]


drawGameOver :: SystemWithResources ()
drawGameOver =
    drawCenteredTexts
        [ TextGameOver
        , TextPressSpaceToContinue
        ]


drawNumber :: ( Show a, Num a ) => V2 CInt -> a -> SystemWithResources ()
drawNumber pos =
    zipWithM_ drawText
        [pos + V2 (0 + i * 16) 0 | i <- [0..]] . numToTextKeys


drawCenteredTexts :: [TextKey] -> SystemWithResources ()
drawCenteredTexts =
    zipWithM_ drawCenteredText [100 * i | i <- [2..]]


drawCenteredText :: CInt -> TextKey -> SystemWithResources ()
drawCenteredText yPos key = do
    texts <- askForTexts
    textW <- SDL.textureWidth <$> SDL.queryTexture (texts HM.! key)
    drawText (V2 ((windowWidth - textW) `div` 2 ) yPos) key


drawText :: V2 CInt -> TextKey -> SystemWithResources ()
drawText pos key = do
    renderer <- askForRenderer
    texts <- askForTexts
    let text = texts HM.! key
    (SDL.TextureInfo _ _ textW textH) <- SDL.queryTexture text
    SDL.copy renderer text Nothing $ Just $
        SDL.Rectangle (SDL.P pos) (V2 textW textH)

