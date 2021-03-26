{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Resources
    ( loadTextures
    , Textures
    , loadTexts
    , Texts
    , TextTexture(..)
    ) where

import Utility

import qualified SDL
import qualified SDL.Image as IMG
import qualified SDL.Font as FNT
import qualified Data.HashMap.Internal.Strict as HashMap
import Data.HashMap.Internal.Strict ((!))
import Linear
import GHC.Generics ( Generic )
import Data.Hashable



-- collection type for all the textures
type Textures = HashMap.HashMap String SDL.Texture

keyPathList :: [(String, FilePath)]
keyPathList =
    [ ("Ship", "resources/ship.png")
    , ("Small", "resources/asteroid.png")
    , ("Bullet", "resources/bullet.png")
    , ("Background", "resources/space-background.png")
    ]

loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer = ioOrDie "Failed to load a texture" $
    HashMap.fromList <$> mapM mapPathToTexture keyPathList
    where
        mapPathToTexture :: (String, FilePath) -> IO (String, SDL.Texture)
        mapPathToTexture = mapM (IMG.loadTexture renderer)


-- collection type for all the rendered texts
type Texts = HashMap.HashMap TextTexture SDL.Texture

data TextTexture = TextPaused | TextPressEscape
    deriving (Eq, Generic, Show)
instance Hashable TextTexture


data FontSize = SmallFont | BigFont
    deriving (Eq, Generic, Show)
instance Hashable FontSize

-- keySizeTextList :: [(TextTexture, (FontSize, text-1.2.4.1:Data.Text.Internal.Text))]
keySizedTextList =
    [ (TextPaused, (BigFont, "PAUSED"))
    , (TextPressEscape, (SmallFont, "Press ESCAPE or SPACE to unpause"))
    ]

loadTexts :: SDL.Renderer -> IO Texts
loadTexts renderer = do
    let fontsList = 
            [ (SmallFont, 32)
            , (BigFont, 64)
            ]
    fonts <- HashMap.fromList <$>
                mapM (mapM (FNT.load "resources/computer-speak.ttf")) fontsList

    textTextureList <- mapM (mapSizedTextToTexture fonts) keySizedTextList
    sequence_ $ HashMap.map FNT.free fonts
    pure $ HashMap.fromList textTextureList

    where
        mapSizedTextToTexture fonts = mapM (sizeAndStringToTexture fonts)
        sizeAndStringToTexture fonts (fontSize, string) = do
            surface <- FNT.solid (fonts ! fontSize) (V4 255 255 255 255) string
            SDL.createTextureFromSurface renderer surface
