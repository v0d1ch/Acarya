{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Server where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import Message.Message
import Network.Wai.Handler.Warp as Warp
import Servant
import Server.Api

server :: STM (Mlist Message) -> Server Api
server mlist =
  addMessagePostH
  where
    addMessagePostH msg = liftIO $ messagePost mlist msg

    messagePost :: STM (Mlist Message) -> Text -> IO Bool
    messagePost ml msg = do
      chan <-
        atomically $ do
          channel <- ml
          void $ addMessage channel (Message msg Info)
          pure channel
      runner chan
      pure True

app :: Application
app = serve api $ server createMlist

mkApp :: IO Application
mkApp = pure app

run :: IO ()
run = Warp.run 4000 =<< mkApp


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
