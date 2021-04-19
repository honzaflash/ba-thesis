{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Resources
-- ( loadResources
-- , runWithResources
--   -- TODO freeResources
-- , Resources
-- , WithResources
-- , EffectsWithResources
-- , SystemWithResources
-- , Textures
-- , Texts
-- , TextTexture(..)
-- ) where
where


import Components ( Velocity, Position, World )
import Utility

import qualified SDL
import qualified SDL.Image as IMG
import qualified SDL.Font as FNT
import qualified Data.HashMap.Internal.Strict as HM
import Data.HashMap.Internal.Strict ((!))
import GHC.Generics ( Generic )
import Data.Hashable ( Hashable )
import Data.Maybe ( mapMaybe )
import qualified Data.String
import Control.Monad.Reader
import Apecs ( SystemT )



-- * Reader monad for resources and accompaning getters

-- | Monad types
type SystemWithResources a = SystemT World WithResources a
type WithResources = ReaderT Resources IO


-- | runReaderT wrapper
runWithResources :: Resources -> WithResources () -> IO ()
runWithResources = flip runReaderT


-- | asks wrappers in System
askForRenderer :: SystemWithResources SDL.Renderer
askForRenderer = lift $ asks resRenderer

askForTextures :: SystemWithResources Textures
askForTextures = lift $ asks resTextures

askForTexts :: SystemWithResources Texts
askForTexts = lift $ asks resTexts

askForRandPosGen :: SystemWithResources (IO Position)
askForRandPosGen = lift $ asks resRandPosGen

askForRandVelGen :: SystemWithResources (IO Velocity)
askForRandVelGen = lift $ asks resRandVelGen

askForRandPos :: SystemWithResources Position
askForRandPos = lift $ asks resRandPosGen >>= liftIO

askForRandVel :: SystemWithResources Velocity
askForRandVel = lift $ asks resRandVelGen >>= liftIO


-- | Resources structure
data Resources =
    Resources
    { resRenderer   :: SDL.Renderer
    , resTextures   :: Textures
    , resTexts      :: Texts
    , resRandPosGen :: IO Position
    , resRandVelGen :: IO Velocity
    }


-- | load/initialize all resources
loadResources :: SDL.Renderer -> IO Resources
loadResources renderer = do
    -- initialize random generators
    time <- round <$> SDL.time
    randGen :: IO Int <- initStatefulRanGen time 0 99999
    [seed1, seed2, seed3, seed4] <- replicateM 4 randGen

    randomPositionGen <-
        initRandomPositionGenerator seed1 seed2
    randomVelocityGen <-
        initRandomVelocityGenerator seed3 seed4

    -- initialize textures
    textureMap <- loadTextures renderer

    -- initialize text textures
    textMap <- loadTexts renderer

    -- wrap it up into Resources
    pure $ Resources
                renderer
                textureMap
                textMap
                randomPositionGen
                randomVelocityGen



-- * Types of resources and their load functions

-- | collection type for all the textures
type Textures = HM.HashMap TextureKey SDL.Texture
type TextureKey = String


loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer = ioOrDie "Failed to load a texture" $
    HM.fromList <$> mapM mapPathToTexture keyPathList
    where
        mapPathToTexture :: (TextureKey, FilePath) -> IO (TextureKey, SDL.Texture)
        mapPathToTexture = mapM (IMG.loadTexture renderer)

        
keyPathList :: [(TextureKey, FilePath)]
keyPathList =
    [ ("Ship", "resources/ship.png")
    , ("Small", "resources/asteroid.png")
    , ("Bullet", "resources/bullet.png")
    , ("Background", "resources/space-background.png")
    ]


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

