{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Components where


import Apecs
import Linear ( V2 )
import Foreign.C.Types ( CDouble, CInt )
import qualified SDL.Input as SDL



-- | Ship components
newtype Ship = Ship { shipAngle :: Angle } deriving Show
instance Component Ship where type Storage Ship = Unique Ship

type Angle = CDouble

newtype ShipLives = ShipLives Int
    deriving Show
instance Component ShipLives where type Storage ShipLives = Global ShipLives
instance Semigroup ShipLives where (<>) = const
instance Monoid ShipLives where mempty = ShipLives 3

data ShipState
    = Alive
    | Exploding Int
    | Respawning Int
    deriving (Show, Eq)
instance Semigroup ShipState where (<>) = const
instance Monoid ShipState where mempty = Alive
instance Component ShipState where type Storage ShipState = Global ShipState


-- | Asteroid component
newtype Asteroid = Asteroid { astSize :: CInt } deriving Show
instance Component Asteroid where type Storage Asteroid = Map Asteroid

type AsteroidComponents = (Asteroid, Kinetic)


-- | Ufo component
data Ufo = Ufo
           { ufoTimeToShoot :: Int
           , ufoSize        :: UfoSize
           } deriving Show
instance Component Ufo where type Storage Ufo = Map Ufo

data UfoSize = SmallSaucer | LargeSaucer deriving Show

type UfoComponents = (Ufo, TimeToLive, Kinetic)


-- | Bullet component
newtype Bullet = Bullet { bulletShotBy :: ShotBy } deriving Show
instance Component Bullet where type Storage Bullet = Map Bullet

data ShotBy
    = ShotByShip
    | ShotByUfo
    deriving (Show, Eq)

type BulletComponents = (Bullet, TimeToLive, Kinetic)


-- | TTL component for bullets and ufos
newtype TimeToLive = Ttl Int
    deriving Show
instance Component TimeToLive where type Storage TimeToLive = Map TimeToLive


-- | Kinetic components
newtype Position = Position (V2 CDouble)
    deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 CDouble)
    deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

type Kinetic = (Position, Velocity)


-- * Global components

-- | Score
newtype Score = Score Int
    deriving (Show, Num)
instance Semigroup Score where (<>) = const
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score


-- | Awarded bonus life points counter
--   serves for detecting the tipping point between the 10000s
newtype LivesAwarded = LivesAwarded Int
    deriving (Show, Num)
instance Semigroup LivesAwarded where (<>) = const
instance Monoid LivesAwarded where mempty = 0
instance Component LivesAwarded where type Storage LivesAwarded = Global LivesAwarded


-- | Wave time counter
newtype WaveTime = WaveTime Int
    deriving (Show, Num)
instance Semigroup WaveTime where (<>) = const
instance Monoid WaveTime where mempty = 0
instance Component WaveTime where type Storage WaveTime = Global WaveTime


-- | Wave number counter
newtype WaveNumber = WaveNumber Int
    deriving (Show, Num)
instance Semigroup WaveNumber where (<>) = const
instance Monoid WaveNumber where mempty = 0
instance Component WaveNumber where type Storage WaveNumber = Global WaveNumber


-- | Wave pause timer for a brief pause after clearing all asteroids
newtype WavePauseTimer = WavePauseTimer Int
    deriving (Show, Num)
instance Semigroup WavePauseTimer where (<>) = const
instance Monoid WavePauseTimer where mempty = 0
instance Component WavePauseTimer
    where type Storage WavePauseTimer = Global WavePauseTimer


-- | State of the main loop
data GameLoopState
    = Playing
    | Paused
    | GameOver
    | InMenu
    | Quit
    deriving (Show, Eq)
instance Semigroup GameLoopState where (<>) = const
instance Monoid GameLoopState where mempty = InMenu
instance Component GameLoopState where type Storage GameLoopState = Global GameLoopState


-- | State variable for input handling
data InputState =
    InputState
    { isHeldW     :: Bool
    , isHeldA     :: Bool
    , isHeldD     :: Bool
    , werePressed :: [SDL.Keycode]
    , quitEvent   :: Bool
    }
    deriving Show
instance Semigroup InputState where
    InputState w1 a1 d1 other1 quit1 <> InputState w2 a2 d2 other2 quit2 =
        InputState w2 a2 d2 (other1 <> other2) (quit1 || quit2)
instance Monoid InputState where
    mempty = InputState False False False [] False
instance Component InputState where type Storage InputState = Global InputState


-- | Apecs template creates the implementation
makeWorld "World" [ ''Ship
                  , ''ShipLives
                  , ''ShipState
                  , ''Ufo
                  , ''Asteroid
                  , ''Bullet
                  , ''TimeToLive
                  , ''Position
                  , ''Velocity
                  , ''Score
                  , ''LivesAwarded
                  , ''WaveTime
                  , ''WaveNumber
                  , ''WavePauseTimer
                  , ''GameLoopState
                  , ''InputState
                  ]

