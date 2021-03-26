module Draw where

import Components
import Resources
import Utility

import Apecs
import qualified SDL
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Internal.Strict ((!))
import Linear
import Foreign.C.Types (CDouble, CInt)


drawScene :: SDL.Renderer -> Textures -> System' ()
drawScene renderer textures = do

    liftIO $ SDL.copy renderer (textures ! "Background") Nothing Nothing
    cmapM_ $ \(Ship a, Position pos) -> drawShip pos a
    cmapM_ $ \(Bullet _, Position pos, Velocity v) -> drawBullet pos v
    cmapM_ $ \(Asteroid size, Position pos) -> drawAsteroid pos size

    where
        drawShip position a = liftIO $
            render "Ship" (position + V2 (-50) (-30), V2 100 60) $ Just $ a / pi * 180
        
        drawBullet position velocity = liftIO $
            render "Bullet" (position + V2 (-15) (-15), V2 30 30) $ Just $ unangle velocity / pi * 180

        drawAsteroid position size = liftIO $
            render "Small" (fmap (subtract (fromIntegral size / 2)) position, pure size) Nothing

        render :: String -> (V2 CDouble, V2 CInt) -> Maybe CDouble -> IO ()
        render textureKey (position, size) Nothing =
            SDL.copy   renderer
                       (textures ! textureKey)
                       Nothing 
                       (Just $ SDL.Rectangle (SDL.P $ fmap round position) size)

        render textureKey (position, size) (Just angle) =
            SDL.copyEx renderer
                       (textures ! textureKey)
                       Nothing
                       (Just $ SDL.Rectangle (SDL.P $ fmap round position) size)
                       angle
                       Nothing $
                       pure False


drawMenu :: SDL.Renderer -> Texts -> IO ()
drawMenu renderer texts = do

    drawText texts 0 TextPaused
    drawText texts 100 TextPressEscape

    where
        drawText texts relY key = do
            let texture = texts ! key
            info <- SDL.queryTexture texture
            let w = SDL.textureWidth info
            let h = SDL.textureHeight info
            SDL.copy renderer texture Nothing $ Just $
                SDL.Rectangle
                    (SDL.P $ (+ V2 0 relY) $ (`div` 2) <$> 
                                V2 (windowWidth - w) (windowHeight - h))
                    (V2 w h)

