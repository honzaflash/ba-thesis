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
import qualified Data.HashMap.Internal.Strict as HashMap
import Data.HashMap.Internal.Strict ((!))
import GHC.Generics ( Generic )
import Data.Hashable
import Control.Monad.Reader
import Linear
import Apecs



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
type Textures = HashMap.HashMap TextureKey SDL.Texture
type TextureKey = String

keyPathList :: [(TextureKey, FilePath)]
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
        mapPathToTexture :: (TextureKey, FilePath) -> IO (TextureKey, SDL.Texture)
        mapPathToTexture = mapM (IMG.loadTexture renderer)


-- collection type for all the rendered texts
type Texts = HashMap.HashMap TextKey SDL.Texture

data TextKey
    = TextPaused
    | TextPressEscape
    deriving (Eq, Generic, Show)
instance Hashable TextKey


data FontSize = SmallFont | BigFont
    deriving (Eq, Generic, Show)
instance Hashable FontSize

-- keySizeTextList :: [(TextKey, (FontSize, text-1.2.4.1:Data.Text.Internal.Text))]
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

