{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import MenuLoop ( menuLoop )
import Resources ( loadTextures, loadTexts )
import Initialize ( initializeWorld )
import Utility

import qualified SDL
import qualified SDL.Image as IMG
import qualified SDL.Font as FNT
import System.Exit (exitSuccess)
import Linear (V2(V2))


main :: IO ()
main = do
    putStrLn "hello, main.hs here"
    SDL.initialize [SDL.InitVideo]
    FNT.initialize

    -- Create a window and renderer
    window <-
        SDL.createWindow "hAsteroids"
             SDL.defaultWindow
            { --SDL.windowBorder = False
              SDL.windowInitialSize = V2 windowWidth windowHeight
            }
    renderer <-
        SDL.createRenderer window (-1)
            SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedRenderer
            , SDL.rendererTargetTexture = False
            }
    -- Display the game
    SDL.showWindow window

    SDL.clear renderer

    -- initialize random generators (TODO seeds)
    randomPosition <- initRandomPositionGenerator 3455 4377
    randomVelocity <- initRandomVelocityGenerator 1545 1253

    textureMap <- loadTextures renderer
    textMap <- loadTexts renderer

    world <- initializeWorld
    menuLoop renderer textMap (textureMap, randomPosition, randomVelocity, world)

    -- TODO destroyTextures
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    IMG.quit
    SDL.quit
    
    exitSuccess
