{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Resources
( loadResources
, runWithResources
  -- TODO freeResources
, Resources
, WithResources
, EffectsWithResources
, Textures
, Texts
, TextTexture(..)
) where


import Components ( Velocity, Position )
import Utility

import qualified SDL
import qualified SDL.Image as IMG
import qualified SDL.Font as FNT
import qualified Data.HashMap.Internal.Strict as HashMap
import Data.HashMap.Internal.Strict ((!))
import GHC.Generics ( Generic )
import Data.Hashable
import Control.Monad.Reader
import Linear



-- * Reader monad for resources and accompaning getters

-- | Resources structure
data Resources =
    Resources
    { resRenderer   :: SDL.Renderer
    , resTextures   :: Textures
    , resTexts      :: Texts
    , resRandPosGen :: IO Position
    , resRandVelGen :: IO Velocity
    }


-- | Reader monad for IO with Resources
type WithResources a = ReaderT Resources IO a
type EffectsWithResources = WithResources ()


-- | asks wrappers
askForRenderer :: WithResources SDL.Renderer
askForRenderer = asks resRenderer

askForTextures :: WithResources Textures
askForTextures = asks resTextures

askForTexts :: WithResources Texts
askForTexts = asks resTexts

askForRandPos :: WithResources Position
askForRandPos = asks resRandPosGen >>= liftIO

askForRandVel :: WithResources Velocity
askForRandVel = asks resRandVelGen >>= liftIO


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


-- | runReader wrapper
runWithResources :: Resources -> EffectsWithResources -> IO ()
runWithResources = flip runReaderT


-- * Types of resources and their load functions

-- | collection type for all the textures
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

