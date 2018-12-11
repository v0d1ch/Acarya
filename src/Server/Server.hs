{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}

module Server.Server where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Network.Simple.TCP as N
import Network.Wai.Handler.Warp as Warp
import Servant
import Server.Api
import Types

server :: STM (Mlist NodeMessage) -> Server Api
server mlist =
  addMessagePostH
  where
    addMessagePostH msg = liftIO $ messagePost mlist msg

    messagePost :: STM (Mlist NodeMessage) -> Text -> IO Bool
    messagePost ml msg = do
      chan <-
        atomically $ do
          channel <- ml
          void $ addMessage channel (NodeMessage (TE.encodeUtf8 msg) Info)
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
  void $ atomically $ readMessage ml
  return ()

spawnAcarya :: MonadIO m => Mlist NodeMessage -> m ()
spawnAcarya ml = do
  void $ liftIO (putStrLn "Launching Acarya push server...")
  N.serve (N.Host "127.0.0.1") "8888" $ \(socket, _remoteAddr) ->
    void $ async $ forever $ do
      msg <- atomically $ readMessage ml -- this is where we send message out
      -- print (nodeMessage msg)
      N.send socket (nodeMessage msg)

spawnClient :: MonadIO m => m ()
spawnClient =
  liftIO $ N.connect "127.0.0.1" "8888" $ \(_connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr

