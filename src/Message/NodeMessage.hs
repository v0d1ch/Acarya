{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Message.NodeMessage where

import Control.Concurrent.STM.TBChan
import Data.Aeson hiding (Error)
import Data.Aeson.Types (typeMismatch)
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

data NodeMessage =
  NodeMessage
  { nodeMessageBody :: Text
  , nodeMessageSeverity :: Severity
  } deriving (Eq, Show)

instance ToJSON NodeMessage where
  toJSON NodeMessage {..} =
    object
      [ "nodeMessageBody" .= nodeMessageBody
      , "nodeMessageSeverity" .= nodeMessageSeverity
      ]

instance FromJSON NodeMessage where
  parseJSON =
    withObject "NodeMessage" $ \o ->
      NodeMessage
      <$> o .: "nodeMessageBody"
      <*> o .: "nodeMessageSeverity"

newtype Mlist a = Mlist { runMlist :: TBChan a }

