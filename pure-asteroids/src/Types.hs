{-# LANGUAGE TemplateHaskell #-}
module Types where


import Linear ( V2 )
-- is .Lazy better? Maybe benchmark this
import qualified Data.HashMap.Strict as HM
import Control.Lens ( makeLenses )


-- | Game world state structure 
data World =
    World
    { _wShip      :: Ship 
    , _wAsteroids :: Asteroids
    , _wBullets   :: Bullets
    , _wUfos      :: Ufos
    , _wWaveTime  :: Time
    , _wWavePause :: Time
    , _wWaveNum   :: Int
    , _wScore     :: Score
    }
    deriving Show


-- | Ship state structure
data Ship =
    Ship 
    { _sPosition :: Position
    , _sVelocity :: Velocity
    , _sAngle    :: Angle
    , _sLives    :: Int
    , _sState    :: ShipState
    }
    deriving Show

data ShipState
    = ShipAlive
    | ShipExploding Time
    | ShipRespawning Time
    deriving Show


-- | Asterodis state structure
type Asteroids = HM.HashMap Int Asteroid
data Asteroid =
    Asteroid
    { _aId       :: Int
    , _aPosition :: Position 
    , _aVelocity :: Velocity
    , _aSize     :: AsteroidSize
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
    { _bId       :: Int
    , _bPosition :: Position
    , _bVelocity :: Velocity
    , _bShooter  :: BulletShooter
    , _bTtl      :: Int
    }
    deriving Show

data BulletShooter = ShotByShip | ShotByUfo deriving (Eq, Show)


-- | Ufos state structure
type Ufos = HM.HashMap Int Ufo
data Ufo =
    Ufo
    { _uId          :: Int
    , _uPosition    :: Position
    , _uVelocity    :: Velocity
    , _uSize        :: UfoSize
    , _uTtl         :: Time -- reaches 0 <--> reaches the end of the screen, and is destroyed
    , _uTimeToShoot :: Time -- when it reaches 0 ufo will shoot
    }
    deriving Show

data UfoSize = LargeSaucer | SmallSaucer
    deriving Show


newtype Score = Score { _sValue :: Int }
    deriving Show


-- | Common elemental properties
newtype Position = Position { _pVect :: V2 Double }
    deriving Show

newtype Velocity = Velocity  { _vVect :: V2 Double }
    deriving Show

type Angle = Double
 
type Time = Int


-- | Structure for event passing between entity groups
data WorldEvents =
    WorldEvents
    { _forAsteroids :: [AsteroidEvent]
    , _forShip      :: [ShipEvent]
    , _forUfos      :: [UfosEvent]
    , _forScore     :: [ScoreEvent]
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


-- | main game loop state
data LoopState
    = Playing
    | GameOver
    | PauseMenu
    | MainMenu
    | QuitGame
    deriving Show


-- | Make all the lenses
makeLenses ''World
makeLenses ''Ship
makeLenses ''Asteroid
makeLenses ''Bullet
makeLenses ''Ufo
makeLenses ''Position
makeLenses ''Velocity
makeLenses ''Score
makeLenses ''WorldEvents

