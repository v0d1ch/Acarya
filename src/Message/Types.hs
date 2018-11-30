{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Message.Types where

import GHC.Generics
import Data.Binary
import Data.Typeable
import Data.Map (Map)
import Control.Distributed.Process (SendPort)
import Control.Concurrent.STM.TBChan

type ClientName = String

newtype JoinMessage = JoinMessage {
    clientName :: ClientName
  } deriving (Generic, Typeable, Show)

instance Binary JoinMessage

data Sender = Server | Client ClientName
  deriving (Generic, Typeable, Eq, Show)

instance Binary Sender

data NodeMessage = NodeMessage {
    nodeMessageSender :: Sender
  , nodeMessage :: String
  , nodeMessageSeverity :: Severity
  } deriving (Generic, Typeable, Show)

instance Binary NodeMessage

type ClientPortMap = Map ClientName (SendPort NodeMessage)

data Severity = Info | Warn | Error deriving (Generic, Typeable, Show)

instance Binary Severity

newtype Mlist a = Mlist { runMlist :: TBChan a }
