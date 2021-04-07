{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Resources
( loadTexts
, Texts
, TextKey (..)
) where


import qualified SDL
import qualified SDL.Font as FNT
import qualified Data.HashMap.Strict as HM
import GHC.Generics ( Generic )
import Data.Hashable ( Hashable )
import qualified Data.String



-- collection type for all the rendered texts
type Texts = HM.HashMap TextKey SDL.Texture

data TextKey
    = TextPaused
    | TextPressSpaceToUnpause
    | TextPressEscapeToExitToMainMenu
    | TextMainMenu
    | TextPressSpaceToStartNewGame
    | TextPressEscapeToQuit
    | TextGameOver
    | TextPressSpaceToContinue
    | TextNumber0
    | TextNumber1
    | TextNumber2
    | TextNumber3
    | TextNumber4
    | TextNumber5
    | TextNumber6
    | TextNumber7
    | TextNumber8
    | TextNumber9
    deriving (Eq, Generic, Show)
instance Hashable TextKey


data FontSize = SmallFont | BigFont
    deriving (Eq, Generic, Show)
instance Hashable FontSize


keySizeTextList :: Data.String.IsString str => [(TextKey, (FontSize, str))]
keySizeTextList =
    [ (TextPaused, (BigFont, "paused"))
    , (TextMainMenu, (BigFont, "main menu"))
    , (TextGameOver, (BigFont, "game over"))
    , (TextPressSpaceToUnpause, (SmallFont, "press space to unpause"))
    , (TextPressEscapeToExitToMainMenu, (SmallFont, "press escape to exit to main menu"))
    -- , (Text, (SmallFont, "")) TODO
    -- , (Text, (SmallFont, ""))
    ]


loadTexts :: SDL.Renderer -> IO Texts
loadTexts renderer = do
    let fontSizes = 
            [ (SmallFont, 32)
            , (BigFont, 64)
            ]
    fonts <- HM.fromList <$> mapM loadFontFromSize fontSizes

    textTextureList <- mapM (mapSizedTextToTexture fonts) keySizeTextList
    sequence_ $ HM.map FNT.free fonts
    pure $ HM.fromList textTextureList

    where
        loadFontFromSize = mapM $ FNT.load "resources/computer-speak.ttf"

        mapSizedTextToTexture fonts = mapM (sizeAndStringToTexture fonts)
        sizeAndStringToTexture fonts (fontSize, string) = do
            surface <- FNT.solid (fonts HM.! fontSize) (SDL.V4 255 255 255 255) string
            SDL.createTextureFromSurface renderer surface

