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
newtype Ship = Ship { getRotation :: Angle } deriving Show
instance Component Ship where type Storage Ship = Unique Ship

type Angle = CDouble

newtype ShipLives = ShipLives Int deriving Show
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
newtype Asteroid = Asteroid { getSize :: CInt } deriving Show
instance Component Asteroid where type Storage Asteroid = Map Asteroid


-- | Bullet component
newtype Bullet = Bullet { getTTL :: Int } deriving Show
instance Component Bullet where type Storage Bullet = Map Bullet

data ShotBy = ShotByShip | ShowtByUfo deriving Show
instance Component ShotBy where type Storage ShotBy = Map ShotBy


-- | Kinetic components
newtype Position = Position (V2 CDouble) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 CDouble) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

type Kinetic = (Position, Velocity)


-- | Global components
newtype Score = Score Int deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score


newtype WorldTime = WorldTime Int deriving (Show, Num)
instance Semigroup WorldTime where (<>) = (+)
instance Monoid WorldTime where mempty = 0
instance Component WorldTime where type Storage WorldTime = Global WorldTime


data GameLoopState
    = Playing
    | Paused
    | GameOver
    | InMenu
    | Quit
    deriving Show
instance Semigroup GameLoopState where (<>) = const
instance Monoid GameLoopState where mempty = InMenu
instance Component GameLoopState where type Storage GameLoopState = Global GameLoopState


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
                  , ''Asteroid
                  , ''Bullet
                  , ''ShotBy
                  , ''Position
                  , ''Velocity
                  , ''Score
                  , ''WorldTime
                  , ''GameLoopState
                  , ''ShipState
                  , ''InputState
                  ]


