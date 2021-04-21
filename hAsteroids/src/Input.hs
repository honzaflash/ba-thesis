{-# LANGUAGE ScopedTypeVariables #-}
module Input
( reactToInput
) where


import Components
import Resources
import Utility ( Time, bulletSpeed, initBulletTtl )

import Apecs
import qualified SDL
import Control.Monad ( void, when )
import Linear
import Foreign.C.Types ( CDouble )



reactToInput :: Time -> SystemWithResources ()
reactToInput dT = do
    events <- map SDL.eventPayload <$> SDL.pollEvents
    modify global $ \(inputState :: InputState) ->
        updateInputState inputState events
    
    (input, loopState, shipState) <- get global
    case (loopState, shipState) of
        (InMenu, _)
            | wasPressed input spaceKeycode  -> set global Playing
            | wasPressed input escapeKeycode -> set global Quit
            | otherwise -> pure ()
        (Paused, _)
            | wasPressed input spaceKeycode  -> set global Playing
            | wasPressed input escapeKeycode -> set global InMenu 
            | otherwise -> pure ()
        (GameOver, _)
            | wasPressed input spaceKeycode  -> set global InMenu
            | otherwise -> pure ()
        (Quit, _) -> pure ()
        (Playing, Exploding _)
            | wasPressed input escapeKeycode -> set global Paused
            | otherwise -> pure ()
        -- Playing and ship is Alive or Respawning
        (Playing, _) ->
            cmapM $ \(Ship a, Velocity vel) ->
                when (wasPressed input escapeKeycode) (set global Paused)
                >> when (wasPressed input spaceKeycode) shoot
                >> pure
                    ( Ship $ a + steering dT input
                    , Velocity $ thrust dT input a vel
                    )


shoot :: SystemWithResources ()
shoot =
    cmapM_ $ \(Ship a, Position pos) -> void $
        newEntity (Bullet ShotByShip
                  , Position pos
                  , Velocity $ bulletSpeed *^ angle a
                  , Ttl initBulletTtl
                  )


-- | Returns a function to modify velocity vector based on input
thrust :: Time -> InputState -> Angle -> V2 CDouble -> V2 CDouble
thrust dT input direction = if isHeldW input 
                               then (+ thrustStrength *^ angle direction)
                               else id
    where thrustStrength = 0.12 * fromIntegral dT


-- | returns delta angle that should be added to the current
--   ship direction, based on the input
steering :: Time -> InputState -> Angle
steering dT input = (if isHeldA input then (-steeringStrength) else 0)
                    + if isHeldD input then steeringStrength else 0
    where steeringStrength = 0.0045 * fromIntegral dT


-- * Input state updating

-- | Takes SDL.Events and updates the InputSate
updateInputState :: InputState -> [SDL.EventPayload] -> InputState
updateInputState state = foldl processEvent resetState
    where resetState = state
                       { werePressed = []
                       , quitEvent = False
                       }


processEvent :: InputState -> SDL.EventPayload -> InputState
processEvent state (SDL.KeyboardEvent eventData) = registerKey eventData state
    where
        registerKey eventData state =
            case eventData of
                (SDL.KeyboardEventData _ _        True _  ) -> state
                (SDL.KeyboardEventData _ SDL.Pressed  _    key) -> press key state
                (SDL.KeyboardEventData _ SDL.Released _    key) -> release key state

        -- if the key is A/W/D set the flag for being held down to True
        -- otherwise add the keycode to the list of keys that were pressed
        press   (SDL.Keysym _ SDL.KeycodeW _) state = state { isHeldW = True }
        press   (SDL.Keysym _ SDL.KeycodeA _) state = state { isHeldA = True }
        press   (SDL.Keysym _ SDL.KeycodeD _) state = state { isHeldD = True }
        press   (SDL.Keysym _ keycode      _) state =
            state { werePressed = keycode : werePressed state }

        -- undo held down flags if key was released
        release (SDL.Keysym _ SDL.KeycodeW _) state = state { isHeldW = False }
        release (SDL.Keysym _ SDL.KeycodeA _) state = state { isHeldA = False }
        release (SDL.Keysym _ SDL.KeycodeD _) state = state { isHeldD = False }
        release _ state = state
processEvent state  SDL.QuitEvent = state { quitEvent = True }
processEvent state  _             = state


-- * Helpers

-- | helper wrapper
spaceKeycode, escapeKeycode :: SDL.Keycode
spaceKeycode = SDL.KeycodeSpace
escapeKeycode = SDL.KeycodeEscape


-- | Helper for querying InputState on one time presses  
wasPressed :: InputState -> SDL.Keycode -> Bool
wasPressed st k = k `elem` werePressed st

