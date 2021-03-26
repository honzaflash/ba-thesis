module GameLoop ( gameLoop ) where

import Input ( reactToInput )
import Step ( stepScene )
import Resources ( Textures )
import Draw ( drawScene )
import Components
import Utility

import qualified SDL
import Apecs ( runWith )
import Control.Monad ( unless )



gameLoop :: SDL.Renderer -> (Textures, IO Position, IO Velocity, World) -> IO ()
gameLoop renderer resources@(textures, randomPosition, randomVelocity, world) = do
    iterationStart <- fromIntegral <$> SDL.ticks

    -- get input
    events <- map SDL.eventPayload <$> SDL.pollEvents

    SDL.clear renderer

    -- run systems
    runWith world $ do
        reactToInput events
        stepScene randomPosition randomVelocity
        drawScene renderer textures

    -- wait if frame was redered too quickly
    iterationTime <- subtract iterationStart . fromIntegral <$> SDL.ticks
    if iterationTime > targetIterationTime
        then putStrLn $ "frame 2 slowww " ++ show iterationTime
        else pure ()
    SDL.delay $ fromIntegral $ max 0 $ targetIterationTime - iterationTime
    
    SDL.present renderer

    let quit = any (keyIsPressed SDL.KeycodeEscape) events
    unless quit $
        gameLoop renderer resources


