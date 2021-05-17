{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where


import GameLoop ( gameLoop )
import Resources ( loadResources, runWithResources, freeResources )
import Components ( initWorld )
import Initialize ( initializeGame )
import Utility

import qualified SDL
import qualified SDL.Image as IMG
import qualified SDL.Font as FNT
import System.Exit ( exitSuccess )
import Linear ( V2(V2) )
import Apecs ( runWith )



main :: IO ()
main = do
    putStrLn "Welcome pilot!"
    SDL.initialize [SDL.InitVideo]
    FNT.initialize

    -- Create a window
    window <-
        SDL.createWindow "hAsteroids"
            SDL.defaultWindow
            { --SDL.windowBorder = False
                SDL.windowInitialSize = V2 windowWidth windowHeight
            }
    SDL.showWindow window

    -- Create a renderer
    renderer <-
        SDL.createRenderer window (-1)
            SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedRenderer
            , SDL.rendererTargetTexture = False
            }
    SDL.clear renderer

    -- load/initialize the resources reader monad
    resources <- loadResources renderer

    -- initialize the game world state variable
    world <- initWorld

    runWithResources resources $
        runWith world $ do
            -- create ship and first asteroid wave
            initializeGame
            -- start the game loop
            gameLoop 0 0

    -- quit
    freeResources resources
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    IMG.quit
    SDL.quit

    putStrLn "Good bye!!"

