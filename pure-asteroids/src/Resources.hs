{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Resources
( loadTexts
, Texts
, TextKey (..)
, numToTextKeys
) where


import qualified SDL
import qualified SDL.Font as FNT
import qualified Data.HashMap.Strict as HM
import GHC.Generics ( Generic )
import Data.Hashable ( Hashable )
import qualified Data.String
import Data.Maybe ( mapMaybe )



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

strNumToTextKey :: Char -> Maybe TextKey
strNumToTextKey str =
    case str of
        '0' -> Just TextNumber0
        '1' -> Just TextNumber1
        '2' -> Just TextNumber2
        '3' -> Just TextNumber3
        '4' -> Just TextNumber4
        '5' -> Just TextNumber5
        '6' -> Just TextNumber6
        '7' -> Just TextNumber7
        '8' -> Just TextNumber8
        '9' -> Just TextNumber9
        _   -> Nothing


data FontSize = SmallFont | MediumFont | BigFont
    deriving (Eq, Generic, Show)
instance Hashable FontSize


loadTexts :: SDL.Renderer -> IO Texts
loadTexts renderer = do
    let fontSizes = 
            [ (SmallFont, 16)
            , (MediumFont, 32)
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


keySizeTextList :: Data.String.IsString str => [(TextKey, (FontSize, str))]
keySizeTextList =
    [ (TextPaused, (BigFont, "paused"))
    , (TextPressSpaceToUnpause, (MediumFont, "press space to unpause"))
    , (TextPressEscapeToExitToMainMenu, (MediumFont, "press escape to exit to main menu"))
    , (TextMainMenu, (BigFont, "main menu"))
    , (TextPressSpaceToStartNewGame, (MediumFont, "press space to start new game"))
    , (TextPressEscapeToQuit, (MediumFont, "press escape to quit"))
    , (TextGameOver, (BigFont, "game over"))
    , (TextPressSpaceToContinue, (MediumFont, "press space to continue"))
    , (TextNumber0, (SmallFont, "0"))
    , (TextNumber1, (SmallFont, "1"))
    , (TextNumber2, (SmallFont, "2"))
    , (TextNumber3, (SmallFont, "3"))
    , (TextNumber4, (SmallFont, "4"))
    , (TextNumber5, (SmallFont, "5"))
    , (TextNumber6, (SmallFont, "6"))
    , (TextNumber7, (SmallFont, "7"))
    , (TextNumber8, (SmallFont, "8"))
    , (TextNumber9, (SmallFont, "9"))
    ]


numToTextKeys :: (Show a, Num a) => a -> [TextKey]
numToTextKeys = mapMaybe strNumToTextKey . show

