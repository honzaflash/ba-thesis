{-# LANGUAGE ScopedTypeVariables #-}
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
import Apecs ( lift, get, global )
import Control.Monad ( unless )



gameLoop :: Time -> Time -> SystemWithResources ()
gameLoop prevTime deltaTime = do

    -- run systems
    reactToInput deltaTime
    stepScene deltaTime
    drawScene

    -- FPS management
    currentTime <- fromIntegral <$> SDL.ticks
    (elapsedTime, delay) <- lockFps targetDeltaTime currentTime

    state :: GameLoopState <- get global
    let quit = case state of
                    Quit -> True 
                    _    -> False
    unless quit $ gameLoop (currentTime + delay) (min 64 $ elapsedTime + delay)

    where
        -- locking fps
        lockFps targetTime currentTime = do
            let elapsedTime = currentTime - prevTime
            let delay = max 0 $ targetTime - elapsedTime
            SDL.delay $ fromIntegral delay
            pure (elapsedTime, delay)

