{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where -- we benchmark this thing



import Input
import Step ( stepScene )
import Resources
import Draw ( drawScene )
import Components
import Utility
import Initialize

import qualified SDL
import qualified SDL.Image as IMG
import qualified SDL.Font as FNT
import Apecs
import Control.Monad
import Linear ( V2(V2) )
import Control.Exception
import System.Environment
import System.Exit ( exitSuccess )



data BenchStats =
    Bench
    { timer    :: Time
    , frameC   :: Int
    , runFor   :: Time
    , lockAt   :: Time
    }
instance Show BenchStats where
    show b = "ran for " ++ show (timer b) ++ "ms; rendered " ++ show (frameC b) ++ " frames"


-- | Benchmark specs
-- runTime, minFrameTime, astCount, lilUfoC, bigUfoC :: Integral a => a
-- runTime = 15000
-- minFrameTime = 1
-- astCount = 100
-- lilUfoC = 10
-- bigUfoC = 30

hackInput :: InputState
hackInput = mempty { isHeldA = True
                   , werePressed = [SDL.KeycodeSpace]
                   }


main :: IO ()
main = do
    args <- getArgs
    when ("-h" `elem` args || null args || length args > 5) $ usage

    let [runTime, minFrameTime, astCount, lilUfoC, bigUfoC] =
            case map readInt args of
                [a] -> [a, 0, 50, 20, 20]
                [a, b] -> [a, b, 50, 20, 20]
                [a, b, c] -> [a, b, c, 20, 20]
                [a, b, c, d] -> [a, b, c, d, d]
                [a, b, c, d, e] -> [a, b, c, d, e]
                _ -> [1000, 0, 50, 20, 20]

    SDL.initialize [SDL.InitVideo]
    FNT.initialize
    
    window <-
        SDL.createWindow "hAsteroids benchmark"
            SDL.defaultWindow
            { SDL.windowInitialSize = SDL.V2 windowWidth windowHeight
            }
    renderer <-
        SDL.createRenderer window (-1)
            SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedRenderer
            , SDL.rendererTargetTexture = False
            }

    SDL.showWindow window

    -- load/initialize the resources reader monad
    resources <- loadResources renderer

    -- initialize the game world state variable
    world <- initWorld

    runWithResources resources $
        runWith world $ do
            prepWorld astCount lilUfoC bigUfoC
            -- start the game loop
            gameLoop' 0 0 (Bench 0 0 runTime minFrameTime)

    freeResources resources
    SDL.destroyWindow window
    IMG.quit
    SDL.quit

    where
        usage = do
            name <- getProgName
            putStrLn $ name ++
                " <runtime> [<min-frame-time> [<ast-count> [<lil-ufo-count> <big-ufo-count>]|[<ufo-count>]]]"
            exitSuccess
        
        readInt :: String -> Int
        readInt str = read str


prepWorld :: Int -> Int -> Int -> SystemWithResources ()
prepWorld astCount lilUfoC bigUfoC = do
    set global (WaveNumber $ astCount - 4)
    newEntity ( Ship $ pi * 3 / 2
              , Position $ V2 (windowWidthF / 2) (windowHeightF / 2)
              , Velocity $ V2 0 0
              )
    spawnNewAsteroidWave
    spawnUfos lilUfoC bigUfoC
    set global (Playing, ShipLives 1000)


spawnUfos :: Int -> Int -> SystemWithResources ()
spawnUfos lilUfoC bigUfoC = do
    replicateM_ lilUfoC $ newUfo SmallSaucer
    replicateM_ bigUfoC $ newUfo LargeSaucer

    where
        newUfo size = do
            (Position (V2 _ posY)) <- askForRandPos
            void $ newEntity ( Ufo 100 size
                             , Position $ V2 (-20) posY
                             , Velocity $ V2 6 0
                             , Ttl 16000
                             )


-- | Hacked main game loop
gameLoop' :: Time -> Time -> BenchStats -> SystemWithResources ()
gameLoop' prevTime deltaTime bench = do

    -- run systems
    reactToInput' deltaTime hackInput
    stepScene deltaTime
    drawScene


    -- FPS management
    currentTime <- fromIntegral <$> SDL.ticks
    (elapsedTime, delay) <- lockFps (lockAt bench) currentTime

    state :: GameLoopState <- get global
    let quit = case state of
                    Quit -> True 
                    _    -> False
    
    if timer bench > (runFor bench)
        then liftIO $ print bench
        else -- Next frame
            gameLoop' (currentTime + delay)
                     (min 64 $ elapsedTime + delay)
                     bench { timer  = timer bench + elapsedTime
                           , frameC = frameC bench + 1
                           }

    where
        -- locking fps
        lockFps targetTime currentTime = do
            let elapsedTime = currentTime - prevTime
            let delay = max 0 $ targetTime - elapsedTime
            SDL.delay $ fromIntegral delay
            pure (elapsedTime, delay)

