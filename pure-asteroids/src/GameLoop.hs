{-# LANGUAGE ScopedTypeVariables #-}
module GameLoop
    ( gameLoop 
    ) where

import Types
import Step ( stepWorld )
import Draw ( drawWorld )

import qualified SDL
import Control.Monad (unless)

gameLoop :: SDL.Renderer -> Time -> World -> IO ()
gameLoop renderer prevTime w = do

    currentTime <- fromIntegral <$> SDL.ticks
    let deltaTime = currentTime - prevTime
    putStr $ " " ++ show deltaTime

    events <- map SDL.eventPayload <$> SDL.pollEvents
    let wEvents = WorldEvents []

    let newW = stepWorld deltaTime wEvents w
    drawWorld renderer newW

    SDL.delay $ fromIntegral $ max 0 $ 16 - deltaTime
    unless (SDL.QuitEvent `elem` events) $
        gameLoop renderer currentTime newW

