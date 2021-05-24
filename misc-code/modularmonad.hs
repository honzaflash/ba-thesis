{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.IO.Class
import Control.Monad.Reader

class MonadReader r m => TexturesReader r m where
    askForTextures :: m Textures

class MonadReader r m => RandomReader r m where
    askForRandom :: m RandGen

type WithResources a = ReaderT Resources IO a

type Textures = String
type RandGen = [Int]
data Resources =
    Res
    { textures :: Textures
    , random   :: RandGen
    }

newtype World = World Int deriving Show

instance Monad m => TexturesReader Resources (ReaderT Resources m) where
    askForTextures = asks textures

instance Monad m => RandomReader Resources (ReaderT Resources m) where
    askForRandom = asks random




main :: IO ()
main = do
    let world = World 10
    let resources = Res  
            { textures = "Current world state: "
            , random = [4, 2]
            }
    runReaderT (game world) resources  


game :: World -> WithResources ()
game w = do
    newW <- step w
    draw w
    quit <- liftIO $ getLine
    unless (quit == "q") $
        game newW


step :: (RandomReader r m) => World -> m World
step (World state)= do
    rand <- head <$> askForRandom
    return $ World $ state + rand


draw :: (MonadIO m, TexturesReader r m) => World -> m ()
draw w = do
    t <- askForTextures
    liftIO $ putStrLn $ t ++ show w


