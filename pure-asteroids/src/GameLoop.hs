module GameLoop
( gameLoop
, LoopState (..)
) where


import Types
import Input
import Resources ( Texts )
import EventProcessing ( processWorldEvents )
import Step ( stepWorld )
import Initialize ( initializeWorld )
import Draw ( drawScene )
import Utility

import qualified SDL
import Control.Monad ( unless )
import Text.Printf ( printf )
import Control.Lens ( (^.) )



-- | Main game loop
gameLoop
    :: SDL.Renderer
    -> Texts
    -> RandomStream Double
    -> Time -- beginnig of the frame computation time stamp
    -> Time -- time to apply to the world
    -> LoopState
    -> InputState
    -> WorldEvents
    -> World
    -> IO ()
gameLoop r texts rand startTime deltaTime loopState prevInput wEvents oldW = do

    SDL.pumpEvents -- update the event queue
    newInput <- processInput prevInput <$> SDL.pollEvents

    -- World updating
    let (newWEvents, newW) = updateWorldIfPlaying newInput loopState
    
    -- World drawing
    drawScene r texts loopState newW 

    -- State transitions
    let newLoopState = nextLoopState newInput newW
    let newW' = resetIfNewGame newLoopState newW

    -- FPS management
    currentTime <- fromIntegral <$> SDL.ticks
    (delay, elapsedTime) <- fmap (min 64) <$> lockFps 16 currentTime

    unless (newInput ^. quitEvent || isQuitGame loopState) $
    -- Next frame
        gameLoop r
                 texts
                 (drop 3 rand) -- dropping 3 that were used in stepWorld
                 (currentTime + delay) -- roughly the time of this call
                 (elapsedTime + delay) -- delta time for next frame
                 newLoopState
                 newInput
                 newWEvents
                 newW'

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
                -- never actually executed because `gameLoop` exits
                QuitGame                                -> loopState

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

        isQuitGame QuitGame = True
        isQuitGame _        = False

