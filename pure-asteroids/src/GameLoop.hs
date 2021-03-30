{-# LANGUAGE ScopedTypeVariables #-}
module GameLoop
    ( gameLoop 
    ) where

import Types
import EventProcessing
import Step ( stepWorld )
import Draw ( drawWorld )

import qualified SDL
import Control.Monad ( unless )
import Text.Printf ( printf )

gameLoop :: SDL.Renderer -> Time -> Time -> WorldEvents -> World -> IO ()
gameLoop renderer prevTime deltaTime wEvents w = do

    sdlEvents <- map SDL.eventPayload <$> SDL.pollEvents

    let (newWEvents, newW) =
            stepWorld deltaTime $ processWorldEvents wEvents w
    
    drawWorld renderer newW

    -- FPS management
    currentTime <- fromIntegral <$> SDL.ticks
    frameTime <- lockFps 16 currentTime

    unless (SDL.QuitEvent `elem` sdlEvents) $
        gameLoop renderer currentTime frameTime newWEvents newW
    
    where
        lockFps targetTime currentTime = do
            let elapsedTime = currentTime - prevTime
            let delay = max 0 $ targetTime - elapsedTime
            SDL.delay $ fromIntegral delay
            -- putStr $ printf "%d/%d " elapsedTime delay -- show ticks
            pure $ elapsedTime + delay


