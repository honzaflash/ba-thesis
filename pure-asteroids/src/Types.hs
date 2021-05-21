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

-- radius of the asteroid
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

data BulletShooter
    = ShotByShip
    | ShotByUfo
    deriving (Eq, Show)

bulletSpeed, ufoBulletSpeed :: Floating a => a
bulletSpeed = 130
ufoBulletSpeed = 80

initBulletTtl, initUfoBulletTtl :: Integral a => a
initBulletTtl = 550
initUfoBulletTtl = 600


-- | Ufos state structure
type Ufos = HM.HashMap Int Ufo
data Ufo =
    Ufo
    { _uId          :: Int
    , _uPosition    :: Position
    , _uVelocity    :: Velocity
    , _uSize        :: UfoSize
    , _uTtl         :: Time -- reaches 0 <=> reaches the end of the screen, and is destroyed
    , _uTimeToShoot :: Time -- when it reaches 0 ufo will shoot
    }
    deriving Show

data UfoSize = LargeSaucer | SmallSaucer
    deriving Show


-- | Score state structure
data Score =
    Score
    { _sValue        :: Int
    , _sLivesAwarded :: Int
    }
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
    , _forUfos      :: [UfoEvent]
    , _forBullets   :: [BulletEvent]
    , _forScore     :: [ScoreEvent]
    }
    deriving Show
instance Semigroup WorldEvents where
    (WorldEvents aE1 sE1 uE1 bE1 scrE1) <> (WorldEvents aE2 sE2 uE2 bE2 scrE2) =
        WorldEvents (aE1 <> aE2) (sE1 <> sE2) (uE1 <> uE2) (bE1 <> bE2) (scrE1 <> scrE2)
instance Monoid WorldEvents where
    mempty = WorldEvents [] [] [] [] []

nullEvents :: WorldEvents -> Bool
nullEvents (WorldEvents [] [] [] [] []) = True
nullEvents _                            = False


-- * Individual event types

-- | Asteroid event with an asteroid's ID
newtype AsteroidEvent = 
    BreakE Int
    deriving Show

-- | Ship event
data ShipEvent = 
    HitE | GainLifeE
    deriving Show

-- | Ufos event with a UFO's ID
newtype UfoEvent = 
    DestroyE Int
    deriving Show

-- | Bullets event with bullet data
data BulletEvent = 
    UfoShootsE Position Velocity
    deriving Show

-- | Score event with number of points
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

