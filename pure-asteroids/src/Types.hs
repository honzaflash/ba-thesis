
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where


import Linear
-- is .Lazy better?
import qualified Data.HashMap.Strict as HM


data World =
    W
    { wShip :: Ship 
    , wAsteroids :: Asteroids
    , wBullets :: Bullets
--    , wAliens :: Aliens
    , wTime :: Time
    , wScore :: Score
    }
    deriving Show

data Ship =
    Ship 
    { shipPosition :: Position 
    , shipVelocity :: Velocity
    , shipAngle :: Angle
    }
    deriving Show

type Asteroids = HM.HashMap Int Asteroid
data Asteroid =
    Asteroid
    { astId :: Int
    , astPosition :: Position 
    , astVelocity :: Velocity
    , astAngle :: Angle
    , astSize :: AsteroidSize
    }
    deriving Show

minAsteroidSize, initAsteroidSize :: Int 
minAsteroidSize = 32
initAsteroidSize = 128

type Bullets = HM.HashMap Int Bullet
data Bullet =
    Bullet
    { bulletId :: Int
    , bulletPosition :: Position
    , bulletVelocity :: Velocity
    , bulletTTL :: Double
    }
    deriving Show

class Kinetics a where
    kmap :: (V2 Double -> V2 Double) -> a -> a

newtype Position = Position { unposition :: V2 Double }
    deriving Show
instance Kinetics Position where
    kmap f (Position p) = Position $ f p

newtype Velocity = Velocity  { unvelocity :: V2 Double }
    deriving Show
instance Kinetics Velocity where
    kmap f (Velocity p) = Velocity $ f p


type Angle = Double
    -- deriving (Show, Eq, Ord)

type AsteroidSize = Int
    -- deriving (Show, Eq, Ord)

type Time = Int
    -- deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

type Score = Int
    -- deriving (Show, Eq, Ord)

-- can be turned into 'data' for addressing other entity types
newtype WorldEvents =
    WorldEvents
    { forAsteroids :: [AsteroidEvent]
    }
    deriving Show

-- can be turned into 'data' for more kinds of events
newtype AsteroidEvent = 
    Destroy Int
    deriving Show

