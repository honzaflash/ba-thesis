module GameLoop
    ( gameLoop 
    ) where


import Types
import Input ( processInput, quitEvent, InputState )
import EventProcessing ( processWorldEvents )
import Step ( stepWorld )
import Draw ( drawWorld )

import qualified SDL
import Control.Monad ( unless )
import Text.Printf ( printf )
import Control.Lens ( (^.) )



-- | Main game loop
gameLoop :: SDL.Renderer -> Time -> Time -> InputState -> WorldEvents -> World -> IO ()
gameLoop renderer prevTime deltaTime prevInput wEvents w = do

    newInput <- processInput prevInput <$> SDL.pollEvents

    -- world updating
    let (newWEvents, newW) =
            stepWorld deltaTime $ processWorldEvents wEvents w
    
    -- world drawing
    drawWorld renderer newW

    -- FPS management
    currentTime <- fromIntegral <$> SDL.ticks
    frameTime <- lockFps 16 currentTime

    unless (newInput ^. quitEvent) $
        gameLoop renderer currentTime frameTime newInput newWEvents newW
    
    where
        lockFps targetTime currentTime = do
            let elapsedTime = currentTime - prevTime
            let delay = max 0 $ targetTime - elapsedTime
            SDL.delay $ fromIntegral delay
            -- putStr $ printf "%d/%d " elapsedTime delay -- show ticks
            pure $ elapsedTime + delay


