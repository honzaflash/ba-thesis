{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Initialize
import GameLoop

import qualified SDL
import Control.Exception


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

    gameLoop renderer 0 16 Playing mempty mempty initializeWorld
        `catch` \(e :: SomeException) -> putStrLn ("whoopsie" ++ show e)

    SDL.destroyWindow window
    SDL.quit

    putStrLn "Good bye!!"

