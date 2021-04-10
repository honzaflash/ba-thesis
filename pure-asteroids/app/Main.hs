{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Initialize
import Resources
import GameLoop
import Utility

import qualified SDL
import qualified SDL.Font as FNT
import Control.Exception


main :: IO ()
main = do
    putStrLn "...starting"

    SDL.initialize [SDL.InitVideo]
    FNT.initialize
    
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

    texts <- loadTexts renderer
    let rand = randStreamGen (0, 100) 420 -- todo better seed

    gameLoop renderer texts rand 0 16 MainMenu mempty mempty initializeWorld
        `catch` \(e :: SomeException) -> putStrLn ("Whoopsie: " ++ show e)

    -- TODO  destroy textures
    SDL.destroyWindow window
    FNT.quit
    SDL.quit

    putStrLn "Good bye!!"

