{-# LANGUAGE OverloadedStrings #-}

module Main where

import Initialize
import GameLoop

import qualified SDL

main :: IO ()
main = do
    putStrLn "...starting"

    SDL.initialize [SDL.InitVideo]
    
    window <-
        SDL.createWindow "hAsteroids"
             SDL.defaultWindow
            { --SDL.windowBorder = False
              SDL.windowInitialSize = SDL.V2 1024 768 --V2 windowWidth windowHeight
            }
    renderer <-
        SDL.createRenderer window (-1)
            SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedRenderer
            , SDL.rendererTargetTexture = False
            }

    SDL.showWindow window

    gameLoop renderer 0 16 mempty initializeWorld

    SDL.destroyWindow window
    SDL.quit

    putStrLn "Good bye!!"



-- TODO remove
keyboardEventTesterThingy = do
    SDL.delay 5000
    SDL.pollEvents >>= mapM print . keyFilter
    SDL.delay 5000
    SDL.pollEvents >>= mapM print . keyFilter
    where
        keyFilter events = [ (SDL.keyboardEventKeyMotion e, SDL.unwrapKeycode $ SDL.keysymKeycode $ SDL.keyboardEventKeysym e) | (SDL.KeyboardEvent e) <- map SDL.eventPayload events ]
