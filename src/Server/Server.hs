{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Server where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import qualified Control.Error as ER
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.Binary as B
import qualified Network.Simple.TCP as N
import Network.Wai.Handler.Warp as Warp
import Servant
import Server.Api
import Types

server :: STM (Mlist NodeMessage) -> Server Api
server mlist =
  spawnPostH :<|>
  addMessagePostH
  where
    spawnPostH h p = liftIO $ spawnPost h p mlist
    addMessagePostH msg = liftIO $ messagePost mlist msg

    spawnPost
      :: Maybe N.HostName
      -> Maybe N.ServiceName
      -> STM (Mlist NodeMessage)
      -> IO Bool
    spawnPost mhost msname ml = do
      serverVars <-
        runMaybeT $ do
          h <- ER.hoistMaybe mhost
          n <- ER.hoistMaybe msname
          return (h, n)
      case serverVars of
        Nothing -> return False
        Just (host, sname) -> do
          chan <- atomically $ do
            channel <- ml
            return channel
          void $ forkIO $ spawnAcarya host sname chan
          pure True

    messagePost :: STM (Mlist NodeMessage) -> String -> IO Bool
    messagePost ml msg = do
      void $
        atomically $ do
          channel <- ml
          void $ addMessage channel (NodeMessage (B.encode msg) Info)
          pure channel
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

spawnAcarya :: MonadIO m => N.HostName -> N.ServiceName -> Mlist NodeMessage -> m ()
spawnAcarya host sname ml = do
  void $ liftIO (putStrLn "Launching Acarya push server...")
  N.serve (N.Host host) sname $ \(socket, _remoteAddr) ->
    void $ async $ forever $ do
      msg <- atomically $ readMessage ml -- this is where we send message out
      print (nodeMessage msg)
      N.sendLazy socket (nodeMessage msg)

spawnClient
  :: MonadIO m
  => N.HostName -> N.ServiceName -> m ()
spawnClient host sname =
  liftIO $
  N.connect host sname $ \(socket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    mmsg <- N.recv socket 1000
    print mmsg

