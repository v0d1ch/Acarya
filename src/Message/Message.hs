{-# LANGUAGE InstanceSigs      #-}

module Message.Message where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad.IO.Class
import Data.HashMap.Lazy
import Data.Text (Text)

type Message = Text

newtype Mlist m a = Mlist { runMlist :: m (TBChan a) }

addMessage :: MonadIO m => Mlist m Message -> Message -> m ()
addMessage ml msg = do
  l <- runMlist ml
  liftIO $ atomically $ writeTBChan l msg

popMessage :: MonadIO m => Mlist m Message -> m Message
popMessage ml = do
  l <- runMlist ml
  liftIO $ atomically $ readTBChan l

class HasMlist a where
  addMsg :: MonadIO m => Mlist m a -> a -> m ()
  readMsg :: MonadIO m => Mlist m a -> m a

instance Monad m => HasMlist (Mlist m a) where
  addMsg ml msg = do
    l <- runMlist ml
    liftIO (atomically $ writeTBChan l msg)

  readMsg ml = do
    l <- runMlist ml
    liftIO (atomically $ readTBChan l)

startMessages :: MonadIO m => Mlist m Message
startMessages = Mlist $ liftIO (newTBChanIO 1000000 :: IO (TBChan Message))

