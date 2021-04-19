module SdlWrappers where


import Resources
import Components

import qualified SDL
import Linear
import Control.Monad.Trans ( lift )
import Foreign.C.Types ( CInt, CDouble )
import Data.HashMap.Strict ( (!) )
import Control.Monad.Reader ( asks )



clearRenderer :: WithResources ()
clearRenderer = asks resRenderer >>= lift . SDL.clear


presentRenderer :: WithResources ()
presentRenderer = asks resRenderer >>= lift . SDL.present


-- | SDL.copy wrapper With Resources
-- | draws a texture with center at @position@ and of size @size@
copyWR :: TextureKey -> V2 CDouble -> V2 CInt -> WithResources ()
copyWR key position size = do
    let position' = round <$> position
    copyWRMaybeRect key $ Just $
        SDL.Rectangle (SDL.P $ position' - fmap (`div` 2) size) size


-- | SDL.copy wrapper With Resources
copyWRMaybeRect :: TextureKey -> Maybe (SDL.Rectangle CInt) -> WithResources ()
copyWRMaybeRect key rect = do
    r <- asks resRenderer
    textures <- asks resTextures
    lift $ SDL.copy r (textures ! key) Nothing rect


-- | SDL.copyEx wrapper With Resources
copyExWR :: TextureKey -> V2 CDouble -> V2 CInt -> Angle -> WithResources ()
copyExWR key position size alfa = do
    let position' = round <$> position
    r <- asks resRenderer
    textures <- asks resTextures
    lift $ SDL.copyEx
                r
                (textures ! key)
                Nothing
                (Just $ SDL.Rectangle
                            (SDL.P $ position' - fmap (`div` 2) size)
                            size)
                alfa
                Nothing $
                pure False

