module Types where

import Control.Concurrent.STM.TBChan
import Data.ByteString (ByteString)

data Severity = Info | Warn | Error deriving (Eq, Show)

data NodeMessage = NodeMessage
  { nodeMessage :: ByteString
  , nodeMessageSeverity :: Severity
  } deriving (Eq, Show)

newtype Mlist a = Mlist { runMlist :: TBChan a }
