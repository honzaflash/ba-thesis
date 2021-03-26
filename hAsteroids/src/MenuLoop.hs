module MenuLoop ( menuLoop ) where


import Components ( Position, Velocity, World )
import Resources ( Textures, Texts )
import Draw ( drawScene, drawMenu )
import Utility

import qualified SDL
import Apecs
import Control.Monad ( unless )
import GameLoop (gameLoop)



menuLoop :: SDL.Renderer -> Texts -> (Textures, IO Position, IO Velocity, World) -> IO ()
menuLoop renderer texts gameLoopResources@(textures, _, _, world) = do
    iterationStart <- fromIntegral <$> SDL.ticks

    -- get input
    events <- map SDL.eventPayload <$> SDL.pollEvents

    SDL.clear renderer

    -- render menu
    runWith world $
        drawScene renderer textures
    drawMenu renderer texts

    -- wait if frame was redered too quickly
    iterationTime <- subtract iterationStart . fromIntegral <$> SDL.ticks
    if iterationTime > targetIterationTime
        then putStrLn $ "frame 2 slowww " ++ show iterationTime
        else pure ()
    SDL.delay $ fromIntegral $ max 0 $ targetIterationTime - iterationTime
    
    SDL.present renderer

    -- "unpause" = run the game loop
    if any (keyIsPressed SDL.KeycodeEscape) events ||
       any (keyIsPressed SDL.KeycodeSpace) events
        then gameLoop renderer gameLoopResources
        else pure ()

    unless (SDL.QuitEvent `elem` events) $
        menuLoop renderer texts gameLoopResources
