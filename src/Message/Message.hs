{-# LANGUAGE InstanceSigs        #-}

module Message.Message where

import Control.Monad (forever, void)
import Control.Concurrent.Async
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad.IO.Class
import Data.HashMap.Lazy
import Data.Text (Text)

data Severity = Info | Warn | Error deriving (Eq, Show)

data Message =
  Message
  { body :: Text
  , severity :: Severity
  } deriving (Eq, Show)

newtype Mlist a = Mlist { runMlist :: TBChan a }

addMessage :: Mlist Message -> Message -> STM ()
addMessage ml = writeTBChan (runMlist ml)

readMessage :: Mlist Message -> STM Message
readMessage ml = readTBChan (runMlist ml)

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

runner :: Mlist Message -> IO ()
runner ml = void $ async $ forever $ do
  msg <- atomically $ readMessage ml -- this is where we send message out
  print msg
