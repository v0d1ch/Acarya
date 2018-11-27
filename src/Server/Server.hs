{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Server where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import Data.Time (UTCTime)
import Message.Message
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS
import Servant
import Server.Api
import System.Environment
import Control.Concurrent.STM

server :: STM (Mlist Message) -> Server Api
server ml =
  addMessagePostH
  where
    addMessagePostH msg = liftIO $ messagePost ml msg

    messagePost :: STM (Mlist Message) -> Text -> IO Bool
    messagePost ml msg = do
      chan <-
        atomically $ do
          mlist <- ml
          void $ addMessage mlist (Message msg Info)
          pure mlist
      runner chan
      pure True

app :: Application
app = serve api $ server createMlist

mkApp :: IO Application
mkApp = pure app

run :: IO ()
run = Warp.run 4000 =<< mkApp


