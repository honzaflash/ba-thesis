module GameLoop
    ( gameLoop 
    ) where


import Types
import Input
import EventProcessing ( processWorldEvents )
import Step ( stepWorld )
import Draw ( drawWorld )

import qualified SDL
import Control.Monad ( unless )
import Text.Printf ( printf )
import Control.Lens ( (^.) )



data LoopState
    = Playing
    | GameOver
    | PauseMenu
    | MainMenu
    | QuitGame
    deriving Show
-- instance Semigroup LoopState where -- TODO
--     Playing  <> other    = other
--     other    <> Playing  = other
--     GameOver <> _        = GameOver
--     -- _        <> GameOver = GameOver


-- | Main game loop
gameLoop
    :: SDL.Renderer
    -> Time
    -> Time
    -> LoopState
    -> InputState
    -> WorldEvents
    -> World
    -> IO ()
gameLoop renderer prevTime deltaTime loopState prevInput wEvents w = do

    newInput <- processInput prevInput <$> SDL.pollEvents

    -- world updating
    let (newWEvents, newW) =
            stepWorld deltaTime newInput $ processWorldEvents wEvents w
    
    -- world drawing
    drawWorld renderer newW

    let newLoopState = nextLoopState newInput newW

    -- FPS management
    currentTime <- fromIntegral <$> SDL.ticks
    frameTime <- min 64 <$> lockFps 16 currentTime

    -- Next frame
    unless (newInput ^. quitEvent) $
        gameLoop renderer currentTime frameTime newLoopState newInput newWEvents newW
    
    where
        nextLoopState input newW =
            case loopState of
                Playing
                    | newW ^. wShip . sState == ShipRespawning &&
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

        lockFps targetTime currentTime = do
            let elapsedTime = currentTime - prevTime
            let delay = max 0 $ targetTime - elapsedTime
            SDL.delay $ fromIntegral delay
            -- putStr $ printf "%d/%d " elapsedTime delay -- show ticks
            pure $ elapsedTime + delay


