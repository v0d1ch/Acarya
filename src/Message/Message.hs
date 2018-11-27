{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Message.Message where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.HashMap.Lazy
import Data.Text (Text)

data Severity = Info | Warn | Error deriving (Eq, Show)

instance ToJSON Severity where
  toJSON = \case
    Info -> String "Info"
    Warn -> String "Warn"
    Error -> String "Error"

instance FromJSON Severity where
  parseJSON = \case
    String "Info" -> return Info
    String "Warn" -> return Warn
    String "Error" -> return Error
    val -> typeMismatch "Severity" val

data Message =
  Message
  { messageBody :: Text
  , messageSeverity :: Severity
  } deriving (Eq, Show)

instance ToJSON Message where
  toJSON Message {..} =
    object
      [ "messageBody" .= messageBody
      , "messageSeverity" .= messageSeverity
      ]

instance FromJSON Message where
  parseJSON =
    withObject "Message" $ \o ->
      Message
      <$> o .: "messageBody"
      <*> o .: "messageSeverity"

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
