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

windowWidth, windowHeight :: CInt
windowWidth = 1024
windowHeight = 768

targetFPS, targetDeltaTime :: Integral a => a
targetFPS = 60
targetDeltaTime = 1000 `div` targetFPS



-- * Helper functions

-- cap velocity to max speed
capVelocity :: CDouble -> Velocity -> Velocity
capVelocity maxSpeed (Velocity vect) = Velocity $
    if norm vect > maxSpeed then maxSpeed *^ signorm vect else vect

-- has key been pressed
keyIsPressed :: SDL.Keycode -> SDL.EventPayload -> Bool
keyIsPressed keycode (SDL.KeyboardEvent event) =
    SDL.keyboardEventKeyMotion event == SDL.Pressed &&
    not (SDL.keyboardEventRepeat event) &&
    SDL.keysymKeycode (SDL.keyboardEventKeysym event) == keycode
keyIsPressed _ _ = False 


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


-- | returns velocity vector with magnitude between 3 and 6
initRandomVelocityGenerator :: Int -> Int -> IO (IO Velocity)
initRandomVelocityGenerator seed1 seed2 =
    fmap Velocity <$>
            initV2generator seed1 seed2 (-5, 5) (-5, 5)
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

