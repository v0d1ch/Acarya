module Types where

import Control.Concurrent.STM.TBChan

type ClientName = String

data Sender = Server | Client ClientName
  deriving (Eq, Show)

data Severity = Info | Warn | Error deriving (Eq, Show)

data NodeMessage = NodeMessage {
    nodeMessageSender :: Sender
  , nodeMessage :: String
  , nodeMessageSeverity :: Severity
  } deriving (Eq, Show)

newtype Mlist a = Mlist { runMlist :: TBChan a }
