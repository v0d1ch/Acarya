{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Message.Message where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad.IO.Class
import Data.HashMap.Lazy
import Data.Text (Text)

data Message =
  Message
  { body :: Text
  } deriving (Eq, Show)

newtype Mlist a = Mlist { runMlist :: (TBChan a) }

addMessage :: Mlist Message -> Message -> STM ()
addMessage ml msg = writeTBChan (runMlist ml) msg

readMessage :: Mlist Message -> STM Message
readMessage ml = readTBChan (runMlist ml)

writeRead :: Message -> IO ()
writeRead m = do
  msg <-
    atomically $ do
      ml <- createMlist
      addMessage ml m
      readMessage ml
  putStrLn (show msg)

class HasMlist a where
  addMsg :: MonadIO m => Mlist a -> a -> m ()
  readMsg :: MonadIO m => Mlist a -> m a

instance HasMlist (Mlist a) where
  addMsg ml msg = liftIO (atomically $ writeTBChan (runMlist ml) msg)
  readMsg ml    = liftIO (atomically $ readTBChan (runMlist ml))
--
createMlist :: STM (Mlist Message)
createMlist = do
  chan <- newTBChan 1000000
  pure $ Mlist chan

