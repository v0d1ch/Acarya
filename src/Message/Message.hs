{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Message.Message where

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

