{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where


import GameLoop ( gameLoop )
import Resources ( loadResources, runWithResources )
import Initialize ( initializeWorld )
import Utility

import qualified SDL
import qualified SDL.Image as IMG
import qualified SDL.Font as FNT
import System.Exit ( exitSuccess )
import Linear ( V2(V2) )
import Apecs ( runWith )


main :: IO ()
main = do
    putStrLn "hello, main.hs here"
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

    -- initialize the game world
    world <- initializeWorld

    -- run the game loop
    runWithResources resources $ runWith world $ gameLoop 0

    -- quit
    -- TODO freeResources
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    IMG.quit
    SDL.quit
    
    exitSuccess
