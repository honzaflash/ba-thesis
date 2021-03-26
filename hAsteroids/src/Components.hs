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
import Foreign.C.Types (CDouble, CInt)

newtype Ship = Ship { getRotation :: CDouble } deriving Show
instance Component Ship where type Storage Ship = Unique Ship

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

data CurrentSceneType = SceneIsGame | SceneIsMenu deriving Show
instance Semigroup CurrentSceneType where (<>) = const
instance Monoid CurrentSceneType where mempty = SceneIsMenu
instance Component CurrentSceneType where type Storage CurrentSceneType = Global CurrentSceneType

makeWorld "World" [ ''Ship
                  , ''Asteroid
                  , ''Bullet
                  , ''Position
                  , ''Velocity
                  , ''Score
                  , ''CurrentSceneType
                  ]

type System' a = System World a


