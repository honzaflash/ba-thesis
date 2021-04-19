module GameLoop
( gameLoop
) where


import Input ( reactToInput )
import Step ( stepScene )
import Resources ( Textures, Texts, SystemWithResources )
import Draw ( drawScene )
import Components
import Utility

import qualified SDL
import Apecs ( lift )
import Control.Monad ( unless )



-- gameLoop :: (SDL.Renderer, Textures, Texts, IO Position, IO Velocity) -> Time -> World -> IO ()
gameLoop :: Time -> SystemWithResources ()
gameLoop prevTime = do
    
    -- get input
    events <- map SDL.eventPayload <$> SDL.pollEvents

    -- run systems
    reactToInput events
    stepScene
    drawScene

    -- FPS management
    currentTime <- fromIntegral <$> SDL.ticks
    deltaTime <- lockFps targetDeltaTime currentTime

    let quit = any (keyIsPressed SDL.KeycodeEscape) events
    unless quit $
        gameLoop deltaTime

    where
        -- locking fps
        lockFps targetTime currentTime = do
            let elapsedTime = currentTime - prevTime
            let delay = max 0 $ targetTime - elapsedTime
            SDL.delay $ fromIntegral delay
            pure $ elapsedTime + delay

