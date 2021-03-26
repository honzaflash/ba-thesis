-- {-# LANGUAGE LambdaCase #-}
module Input where


import Components

import Apecs
import qualified SDL
import Control.Monad (void)
import Linear


reactToInput :: [SDL.EventPayload] -> System' ()
reactToInput events = do
    keyboarState <- SDL.getKeyboardState
    mapM_ (doTrue . fmap keyboarState) actions

    mapM_ handleEvents events

    where
        actions =
            [ (turnLeft, SDL.ScancodeA)
            , (turnRight, SDL.ScancodeD)
            , (propel,   SDL.ScancodeW)
            ]
        doTrue (system, True)  = system
        doTrue (_,      False) = pure ()


handleEvents :: SDL.EventPayload -> System' ()
handleEvents (SDL.KeyboardEvent event) =
    if SDL.keyboardEventKeyMotion event == SDL.Pressed &&
        not (SDL.keyboardEventRepeat event)
        then handleKeypress $
            SDL.keysymKeycode $
            SDL.keyboardEventKeysym event
        else pure ()
    where
        -- Define actions taken for each keycode here
        handleKeypress keycode
            | keycode == SDL.KeycodeSpace = shoot
           --  | keycode == SDL.KeycodeEscape = changeSceneType
            | otherwise = pure ()
handleEvents _ = pure ()


shoot :: System' ()
shoot = cmapM_ $ \(Ship a, Position pos) -> 
                    void $ newEntity (Bullet 40, Position pos, Velocity $ 15 *^ angle a)

turnLeft :: System' ()
turnLeft = cmap $ \(Ship a) -> Ship $ a - 0.085

turnRight :: System' ()
turnRight = cmap $ \(Ship a) -> Ship $ a + 0.085

propel :: System' ()
propel = cmap $ \(Ship a, Velocity v) -> Velocity $ v + 0.6 *^ angle a

-- changeSceneType :: System' ()
-- changeSceneType = modify global $ \case
--                                      SceneIsGame -> SceneIsMenu
--                                      SceneIsMenu -> SceneIsGame

