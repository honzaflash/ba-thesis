{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where -- we benchmark this thing


import Resources
import Utility
import Types
import Input
import EventProcessing ( processWorldEvents )
import Step ( stepWorld )
import Initialize
import Draw ( drawScene )
import Linear

import qualified SDL
import qualified SDL.Font as FNT
import qualified Data.HashMap.Strict as HM
import Text.Printf ( printf )
import Control.Monad
import Control.Lens
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
hackInput = mempty
              & isHeldA .~ True
              & werePressed .~ [spaceKeycode]


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
        SDL.createWindow "pure-asteroids benchmark"
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

    texts <- loadTexts renderer

    let seed = 777
    let rand = randStreamGen (0, 100) $ seed

    gameLoop'
            renderer
            texts
            rand
            0
            16
            Playing
            mempty
            mempty
            (prepWorld astCount lilUfoC bigUfoC)
            (Bench 0 0 runTime minFrameTime)
        `catch` \(e :: SomeException) -> putStrLn ("Whoopsie: " ++ show e)

    destroyTexts texts
    SDL.destroyWindow window
    SDL.destroyRenderer renderer
    FNT.quit
    SDL.quit

    where
        usage = do
            name <- getProgName
            putStrLn $ name ++
                " <runtime> [<min-frame-time> [<ast-count> [<lil-ufo-count> <big-ufo-count>]|[<ufo-count>]]]"
            exitSuccess
        
        readInt :: String -> Int
        readInt str = read str


prepWorld :: Int -> Int -> Int -> World
prepWorld astCount lilUfoC bigUfoC= 
    World
    { _wShip      = initializeShip 
                            & sLives +~ 1000
                            & sVelocity . vVect +~ V2 0 2
    , _wAsteroids = safeRandomAsteroidsSpawn 420 initializeShip astCount
    , _wBullets   = HM.empty
    , _wUfos      = spawnUfos lilUfoC bigUfoC
    , _wWaveTime  = 0
    , _wWavePause = 0
    , _wWaveNum   = 1
    , _wScore     = Score 0 0
    }


spawnUfos :: Int -> Int -> Ufos
spawnUfos small large =
    HM.fromList $
        [(uid, newUfo uid SmallSaucer (uid * uid * 391)) | uid <- [0..small - 1]] ++
        [(uid, newUfo uid LargeSaucer (uid * uid * 241)) | uid <- [small..large]]
    where
        newUfo uid s y =
            let startY = fromIntegral $ y `mod` windowHeight
            in Ufo
               { _uId = uid
               , _uPosition = Position $ V2 0 startY
               , _uVelocity = Velocity $ V2 6 0
               , _uSize = s
               , _uTtl = 16000
               , _uTimeToShoot = 100
               }


-- | Hacked main game loop
gameLoop'
    :: SDL.Renderer
    -> Texts
    -> RandomStream Double
    -> Time -- beginnig of the frame computation time stamp
    -> Time -- time to apply to the world
    -> LoopState
    -> InputState
    -> WorldEvents
    -> World
    -> BenchStats
    -> IO ()
gameLoop' r texts rand startTime deltaTime loopState prevInput wEvents oldW bench = do

    newInput <- processInput prevInput <$> SDL.pollEvents

    -- World updating
    let (newWEvents, newW) = updateWorldIfPlaying hackInput loopState
    
    -- World drawing
    drawScene r texts loopState newW 

    -- State transitions
    let newLoopState = nextLoopState newInput newW
    let newW' = resetIfNewGame newLoopState newW

    -- FPS management
    currentTime <- fromIntegral <$> SDL.ticks
    (delay, elapsedTime) <- fmap (min 64) <$> lockFps (lockAt bench) currentTime

    if (newInput ^. quitEvent || timer bench > (runFor bench))
        then print bench
        else -- Next frame
            gameLoop' r
                  texts
                  (drop 3 rand) -- dropping 3 that were used in stepWorld
                  (currentTime + delay) -- roughly the time of this call
                  (elapsedTime + delay) -- delta time for next frame
                  newLoopState
                  newInput
                  newWEvents
                  newW'
                  bench { timer  = timer bench + elapsedTime
                        , frameC = frameC bench + 1
                        }

    where
        -- update world only if loop is in 'Playing' state 
        updateWorldIfPlaying newInput Playing =
            stepWorld deltaTime newInput rand $
                processWorldEvents wEvents oldW
        updateWorldIfPlaying newInput _       = (mempty, oldW)

        -- State transition function
        nextLoopState input newW =
            case loopState of
                Playing
                    | isRespawning (newW ^. wShip . sState)
                         && newW ^. wShip . sLives <= 0 -> GameOver
                    | wasPressed input escapeKeycode    -> PauseMenu
                    | otherwise                         -> loopState
                PauseMenu
                    | wasPressed input spaceKeycode     -> Playing
                    | wasPressed input escapeKeycode    -> MainMenu
                    | otherwise                         -> loopState
                GameOver
                    | wasPressed input spaceKeycode     -> MainMenu
                    | otherwise                         -> loopState
                MainMenu
                    | wasPressed input spaceKeycode     -> Playing
                    | wasPressed input escapeKeycode    -> QuitGame
                    | otherwise                         -> loopState
        isRespawning (ShipRespawning _) = True
        isRespawning  _                 = False

        -- if transitioning from main menu to playing reinitialize the world
        resetIfNewGame newLoopState =
            case (loopState, newLoopState) of
                (MainMenu, Playing) -> const initializeWorld
                _                   -> id

        -- locking fps
        lockFps targetTime currentTime = do
            let elapsedTime = currentTime - startTime
            let delay = max 0 $ targetTime - elapsedTime
            SDL.delay $ fromIntegral delay
            pure (delay, elapsedTime)

