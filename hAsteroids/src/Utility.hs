module Utility where


import Components

import qualified SDL
import Foreign.C.Types (CDouble, CInt (CInt))
import Control.Monad
import Control.Exception
import System.IO ( stderr, hPrint )
import System.Exit ( die ) 
import System.Random.Stateful
import Data.List
import Linear



-- * Types

type Time = Int 


-- * Global static variables

windowWidth, windowHeight :: Integral a => a
windowWidth = 1024
windowHeight = 768

windowWidthF, windowHeightF :: Floating a => a
windowWidthF = fromIntegral windowWidth
windowHeightF = fromIntegral windowHeight

targetFPS, targetDeltaTime :: Integral a => a
targetFPS = 60
targetDeltaTime = 1000 `div` targetFPS

bulletSpeed, ufoBulletSpeed :: Floating a => a
bulletSpeed = 130
ufoBulletSpeed = 80

initBulletTtl, initUfoBulletTtl :: Integral a => a
initBulletTtl = 550
initUfoBulletTtl = 600

-- diameter of the asteroid
initAsteroidSize :: Integral a => a
initAsteroidSize = 128


-- * stateful random generator initializers

-- initialize a random number generator
initStatefulRanGen :: UniformRange a => Int -> a -> a -> IO (IO a)
initStatefulRanGen seed a b = do
    generator <- newIOGenM $ mkStdGen seed
    let generate = uniformRM (a, b)
    pure $ generate generator

-- initialize 2D vector generator
initV2generator :: UniformRange a => Int -> Int -> (a, a) -> (a, a) -> IO (IO (V2 a))
initV2generator  seed1 seed2 (a1, b1) (a2, b2) = 
    liftM2 (liftM2 V2)
           (initStatefulRanGen seed1 a1 b1)
           (initStatefulRanGen seed2 a2 b2)

initRandomPositionGenerator :: Int -> Int -> IO (IO Position)
initRandomPositionGenerator seed1 seed2 =
    fmap Position <$>
            initV2generator seed1
                            seed2 
                            (0, fromIntegral windowWidth)
                            (0, fromIntegral windowHeight)


-- | returns velocity vector with magnitude between 3 and 8
initRandomVelocityGenerator :: Int -> Int -> IO (IO Velocity)
initRandomVelocityGenerator seed1 seed2 =
    fmap (Velocity . addLength) <$>
            initV2generator seed1 seed2 (-3, 3) (-3, 3)
    where
        addLength vect = vect + 3 *^ signorm vect


-- * IO Exception handlers

-- handle every nonfatal exception
ioOrDefault :: a -> IO a -> IO a
ioOrDefault def expr =
    expr `catch` \e -> do
                         hPrint stderr (e :: SomeException)
                         pure def

-- handle every fatal exception
ioOrDie :: String -> IO a -> IO a
ioOrDie msg expr =
    expr `catch` \e -> do
                         hPrint stderr (e :: SomeException)
                         die msg

