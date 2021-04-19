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



newtype Ship = Ship { getRotation :: Angle } deriving Show
instance Component Ship where type Storage Ship = Unique Ship

type Angle = CDouble

newtype Asteroid = Asteroid { getSize :: CInt } deriving Show
instance Component Asteroid where type Storage Asteroid = Map Asteroid

newtype Bullet = Bullet { getTTL :: Int } deriving Show
instance Component Bullet where type Storage Bullet = Map Bullet

newtype Position = Position (V2 CDouble) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 CDouble) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

type Kinetic = (Position, Velocity)

newtype Score = Score Int deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score

newtype WorldTime = WorldTime Int deriving (Show, Num)
instance Semigroup WorldTime where (<>) = (+)
instance Monoid WorldTime where mempty = 0
instance Component WorldTime where type Storage WorldTime = Global WorldTime

data GameLoopState
    = Playing ShipState
    | Paused
    | GameOver
    | InMenu
    | Quit
    deriving Show
instance Semigroup GameLoopState where (<>) = const
instance Monoid GameLoopState where mempty = InMenu
instance Component GameLoopState where type Storage GameLoopState = Global GameLoopState

data ShipState
    = Alive
    | Exploding
    | Respawning
    deriving Show


makeWorld "World" [ ''Ship
                  , ''Asteroid
                  , ''Bullet
                  , ''Position
                  , ''Velocity
                  , ''Score
                  , ''WorldTime
                  , ''GameLoopState
                  ]


