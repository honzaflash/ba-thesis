{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where


import Linear
-- is .Lazy better? Maybe benchmark this
import qualified Data.HashMap.Strict as HM
import Control.Lens


-- | Game world state structure 
data World =
    World
    { _wShip :: Ship 
    , _wAsteroids :: Asteroids
    , _wBullets :: Bullets
    , _wUfos :: Ufos
    , _wTime :: Time
    , _wScore :: Score
    }
    deriving Show


-- | Ship state structure
-- TODO  add a flag for when the ship is hit,
-- explodes and is invincible for a short time after respawn
data Ship =
    Ship 
    { _sPosition :: Position
    , _sVelocity :: Velocity
    , _sAngle :: Angle
    , _sLives :: Int
    }
    deriving Show


-- | Asterodis state structure
type Asteroids = HM.HashMap Int Asteroid
data Asteroid =
    Asteroid
    { _aId :: Int
    , _aPosition :: Position 
    , _aVelocity :: Velocity
    , _aAngle :: Angle
    , _aSize :: AsteroidSize
    }
    deriving Show

type AsteroidSize = Int

minAsteroidSize, initAsteroidSize :: Int 
minAsteroidSize = 16
initAsteroidSize = 64


-- | Bullets state structure
type Bullets = HM.HashMap Int Bullet
data Bullet =
    Bullet
    { _bId :: Int
    , _bPosition :: Position
    , _bVelocity :: Velocity
    , _bShooter :: BulletShooter
    , _bTtl :: Int
    }
    deriving Show

data BulletShooter = ShotByShip | ShotByUfo deriving (Eq, Show)


-- | Ufos state structure
type Ufos = HM.HashMap Int Ufo
data Ufo =
    Ufo
    { _uId :: Int
    , _uPosition :: Position
    , _uVelocity :: Velocity
    , _uSize :: UfoSize
    , _uTtl :: Time
    , _uTimeToShoot :: Time
    }
    deriving Show

data UfoSize = LargeSaucer | SmallSaucer
    deriving Show


-- | Common elemental properties
newtype Position = Position { _pVect :: V2 Double }
    deriving Show

newtype Velocity = Velocity  { _vVect :: V2 Double }
    deriving Show

type Angle = Double
 
type Time = Int

type Score = Int


-- | Structure for event passing between entity groups
-- TODO  add event type for communicating with the game loop
-- displaying game over screen etc.
data WorldEvents =
    WorldEvents
    { _forAsteroids :: [AsteroidEvent]
    , _forShip :: [ShipEvent]
    , _forUfos :: [UfosEvent]
    , _forScore :: [ScoreEvent]
    }
    deriving Show
instance Semigroup WorldEvents where
    (WorldEvents ae1 se1 ue1 scre1) <> (WorldEvents ae2 se2 ue2 scre2) =
        WorldEvents (ae1 <> ae2) (se1 <> se2) (ue1 <> ue2) (scre1 <> scre2)
instance Monoid WorldEvents where
    mempty = WorldEvents [] [] [] []


nullEvents :: WorldEvents -> Bool
nullEvents (WorldEvents [] [] [] []) = True
nullEvents _                         = False


-- * Individual event types

-- | Asteroid event
newtype AsteroidEvent = 
    BreakE Int
    deriving Show

-- | Ship event
data ShipEvent = 
    HitE | GainLifeE
    deriving Show

-- | Ufos event
newtype UfosEvent = 
    DestroyE Int
    deriving Show

-- | Score event
newtype ScoreEvent = 
    IncreaseE Int
    deriving Show


makeLenses ''World
makeLenses ''Ship
makeLenses ''Asteroid
makeLenses ''Bullet
makeLenses ''Ufo
makeLenses ''Position
makeLenses ''Velocity
makeLenses ''WorldEvents


