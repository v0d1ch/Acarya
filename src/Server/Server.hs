{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}

module Server.Server where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Message.Types
import Network.Wai.Handler.Warp as Warp
import Servant
import Server.Api
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

server :: STM (Mlist NodeMessage) -> Server Api
server mlist =
  addMessagePostH
  where
    addMessagePostH msg = liftIO $ messagePost mlist msg

    messagePost :: STM (Mlist NodeMessage) -> String -> IO Bool
    messagePost ml msg = do
      chan <-
        atomically $ do
          channel <- ml
          void $ addMessage channel (NodeMessage Server msg Info)
          pure channel
      runner chan
      pure True

app :: Application
app = serve api $ server createMlist

mkApp :: IO Application
mkApp = pure app

run :: IO ()
run = Warp.run 4000 =<< mkApp


addMessage :: Mlist NodeMessage -> NodeMessage -> STM ()
addMessage ml = writeTBChan (runMlist ml)

readMessage :: Mlist NodeMessage -> STM NodeMessage
readMessage ml = readTBChan (runMlist ml)

class HasMlist a where
  addMsg :: MonadIO m => Mlist a -> a -> m ()
  readMsg :: MonadIO m => Mlist a -> m a

instance HasMlist (Mlist a) where
  addMsg ml msg = liftIO (atomically $ writeTBChan (runMlist ml) msg)
  readMsg ml    = liftIO (atomically $ readTBChan (runMlist ml))
--
createMlist :: STM (Mlist NodeMessage)
createMlist = do
  chan <- newTBChan 1000000
  pure $ Mlist chan

runner :: Mlist NodeMessage -> IO ()
runner ml = void $ async $ forever $ do
  msg <- atomically $ readMessage ml -- this is where we send message out
  print msg

test :: IO ()
test = do
  transport <- createTransport "127.0.0.1" "4001" ("127.0.0.1",) defaultTCPParameters
  case transport of
    Left e -> putStrLn (show e)
    Right t -> do
      node <- newLocalNode t initRemoteTable
      _ <- runProcess node $ do
        -- get the id of this process
        self <- getSelfPid
        send self "Talking to myself"
        message <- expect :: Process String
        liftIO $ putStrLn message
      return ()
  return ()

