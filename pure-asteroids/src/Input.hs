{-# LANGUAGE TemplateHaskell #-}
module Input where


import qualified SDL
import SDL
    ( Keysym (Keysym, keysymKeycode)
    , EventPayload (KeyboardEvent, QuitEvent)
    , InputMotion (Released, Pressed)
    , KeyboardEventData
        ( KeyboardEventData
        , keyboardEventKeyMotion
        , keyboardEventKeysym
        )
    ) 
import Control.Lens



data InputState =
    InputState
    { _isHeldW :: Bool
    , _isHeldA :: Bool
    , _isHeldD :: Bool
    , _werePressed :: [SDL.Keycode]
    , _quitEvent :: Bool
    }

instance Semigroup InputState where
    InputState w1 a1 d1 other1 quit1 <> InputState w2 a2 d2 other2 quit2 =
        InputState w2 a2 d2 (other1 <> other2) (quit1 || quit2)
instance Monoid InputState where
    mempty = InputState False False False [] False

makeLenses ''InputState


-- | helper wrapper
spaceKeycode :: SDL.Keycode
spaceKeycode = SDL.KeycodeSpace

-- | Helper for querying InputState on one time presses  
wasPressed :: InputState -> SDL.Keycode -> Bool
wasPressed st k = k `elem` st ^. werePressed


-- | Takes SDL.Events and updates the InputSate
processInput :: InputState -> [SDL.Event] -> InputState
processInput state = processNewInput resetState
    where resetState = state
                         & werePressed .~ []
                         & quitEvent .~ False


-- here the actual updating of InputState is done
-- (note some of the SDL functions here were imported "unqualifiedly")
processNewInput :: InputState -> [SDL.Event] -> InputState
processNewInput state = foldl registerEvent state . map SDL.eventPayload
    where
        registerEvent state event =
            case event of
                SDL.KeyboardEvent eventData -> registerKey eventData state
                SDL.QuitEvent -> state & quitEvent .~ True
                _ -> state
        
        registerKey eventData state =
            case eventData of
                (KeyboardEventData _ _        True _  ) -> state
                (KeyboardEventData _ Pressed  _    key) -> press key state
                (KeyboardEventData _ Released _    key) -> release key state

        -- if the key is A/W/D set the flag for being held down
        -- otherwise add the keycode to the list of keys that were pressed
        press   (Keysym _ SDL.KeycodeW _) state = state & isHeldW .~ True
        press   (Keysym _ SDL.KeycodeA _) state = state & isHeldA .~ True
        press   (Keysym _ SDL.KeycodeD _) state = state & isHeldD .~ True
        press   (Keysym _ kc           _) state = state & werePressed %~ (kc :)

        release (Keysym _ SDL.KeycodeW _) state = state & isHeldW .~ False
        release (Keysym _ SDL.KeycodeA _) state = state & isHeldA .~ False
        release (Keysym _ SDL.KeycodeD _) state = state & isHeldD .~ False
        release _ state = state

