module Draw where


import Components
import Resources
import SdlWrappers
import Utility

import Apecs
import qualified SDL
import Data.HashMap.Internal.Strict ((!))
import Linear
import Foreign.C.Types (CDouble, CInt)



-- TODO  GameLoopState ->
drawScene :: SystemWithResources ()
drawScene = do 
    lift clearRenderer
    drawWorld
    lift presentRenderer


drawWorld :: SystemWithResources ()
drawWorld = do
    textures <- askForTextures
    drawBackground
    drawShip
    drawBullets
    drawAsteroids


drawShip :: SystemWithResources ()
drawShip =
    cmapM_ $ \(Ship a, Position pos) -> lift $
        copyExWR "Ship" pos (V2 30 60) $ a / pi * 180
        

drawBullets :: SystemWithResources ()
drawBullets =
    cmapM_ $ \(Bullet _, Position pos, Velocity vel) -> lift $
        copyExWR "Bullet" pos (V2 10 20) $ unangle vel / pi * 180


drawAsteroids :: SystemWithResources ()
drawAsteroids =
    cmapM_ $ \(Asteroid size, Position pos) -> lift $
        copyWR "Small" pos $ pure size


drawBackground :: SystemWithResources ()
drawBackground = lift $ copyWRMaybeRect "Background" Nothing


drawMenu :: SystemWithResources ()
drawMenu = do
    renderer <- askForRenderer
    texts <- askForTexts

    drawText renderer texts 0 TextPaused
    drawText renderer texts 100 TextPressEscape

    where
        drawText renderer texts relY key = do
            let texture = texts ! key
            info <- SDL.queryTexture texture
            let w = SDL.textureWidth info
            let h = SDL.textureHeight info
            SDL.copy renderer texture Nothing $ Just $
                SDL.Rectangle
                    (SDL.P $ (+ V2 0 relY) $ (`div` 2) <$> 
                                V2 (windowWidth - w) (windowHeight - h))
                    (V2 w h)

