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
    putStrLn "Welcome pilot!"

    SDL.initialize [SDL.InitVideo]
    FNT.initialize

    window <-
        SDL.createWindow "pure-asteroids"
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

    seed <- round <$> SDL.time
    let rand = randStreamGen (0, 100) seed

    gameLoop renderer texts rand 0 16 MainMenu mempty mempty initializeWorld
        `catch` \(e :: SomeException) -> putStrLn ("Whoopsie: " ++ show e)

    destroyTexts texts
    SDL.destroyWindow window
    SDL.destroyRenderer renderer
    FNT.quit
    SDL.quit

    putStrLn "Good bye!!"

