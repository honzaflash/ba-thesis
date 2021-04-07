module GameLoop
( gameLoop
, LoopState (..)
) where


import Types
import Input
import Resources
import EventProcessing ( processWorldEvents )
import Step ( stepWorld )
import Draw

import qualified SDL
import Control.Monad ( unless )
import Text.Printf ( printf )
import Control.Lens ( (^.) )



-- | Main game loop
gameLoop
    :: SDL.Renderer
    -> Texts
    -> Time
    -> Time
    -> LoopState
    -> InputState
    -> WorldEvents
    -> World
    -> IO ()
gameLoop r texts prevTime deltaTime loopState prevInput wEvents oldW = do

    newInput <- processInput prevInput <$> SDL.pollEvents

    -- World updating
    let (newWEvents, newW) = updateWorlIfPlaying newInput loopState
    
    -- World drawing
    drawScene r texts loopState newW 

    -- State transitions
    let newLoopState = nextLoopState newInput newW

    -- FPS management
    currentTime <- fromIntegral <$> SDL.ticks
    frameTime <- min 64 <$> lockFps 16 currentTime

    -- Next frame
    unless (newInput ^. quitEvent || isQuitGame loopState) $
        gameLoop r texts currentTime frameTime newLoopState newInput newWEvents newW
    
    where
        -- update world only if loop is in 'Playing' state 
        updateWorlIfPlaying newInput Playing =
            stepWorld deltaTime newInput $
                processWorldEvents wEvents oldW
        updateWorlIfPlaying newInput _       = (mempty, oldW)

        -- State transition function
        nextLoopState input newW =
            case loopState of
                Playing
                    | isRespawning (newW ^. wShip . sState) &&
                        newW ^. wShip . sLives == 0  -> GameOver
                    | wasPressed input escapeKeycode -> PauseMenu
                    | otherwise                      -> loopState
                PauseMenu
                    | wasPressed input spaceKeycode  -> Playing
                    | wasPressed input escapeKeycode -> MainMenu
                    | otherwise                      -> loopState
                GameOver
                    | wasPressed input spaceKeycode  -> MainMenu
                    | otherwise                      -> loopState
                MainMenu
                    | wasPressed input spaceKeycode  -> PauseMenu
                    | wasPressed input escapeKeycode -> QuitGame
                    | otherwise                      -> loopState
        isRespawning (ShipRespawning _) = True
        isRespawning  _                 = False

        -- locking fps
        lockFps targetTime currentTime = do
            let elapsedTime = currentTime - prevTime
            let delay = max 0 $ targetTime - elapsedTime
            SDL.delay $ fromIntegral delay
            -- putStr $ printf "%d/%d " elapsedTime delay -- show ticks
            pure $ elapsedTime + delay

        isQuitGame QuitGame = True
        isQuitGame _        = False

