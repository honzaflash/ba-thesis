
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
    , shipLives :: Int
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

type AsteroidSize = Int

minAsteroidSize, initAsteroidSize :: Int 
minAsteroidSize = 32
initAsteroidSize = 128

type Bullets = HM.HashMap Int Bullet
data Bullet =
    Bullet
    { bulletId :: Int
    , bulletPosition :: Position
    , bulletVelocity :: Velocity
    , bulletShooter :: BulletShooter
    , bulletTTL :: Double
    }
    deriving Show

data BulletShooter = ShotByShip | ShotByUfo deriving Show

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

type Time = Int

type Score = Int


data WorldEvents =
    WorldEvents
    { forAsteroids :: [AsteroidEvent]
    , forShip :: [ShipEvent]
    , forUfos :: [UfosEvent]
    , forScore :: [ScoreEvent]
    }
    deriving Show
instance Semigroup WorldEvents where
    (WorldEvents ae1 se1 ue1 scre1) <> (WorldEvents ae2 se2 ue2 scre2) =
        WorldEvents (ae1 <> ae2) (se1 <> se2) (ue1 <> ue2) (scre1 <> scre2)
instance Monoid WorldEvents where
    mempty = WorldEvents [] [] [] []

-- TODO ?
-- worldEvents :: WorldEvent e => e -> WorldEvents


newtype AsteroidEvent = 
    BreakE Int
    deriving Show

data ShipEvent = 
    HitE | GainLifeE
    deriving Show

newtype UfosEvent = 
    DestroyE Int
    deriving Show

newtype ScoreEvent = 
    IncreaseE Int
    deriving Show

