{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Api where

import Data.Proxy
import qualified Network.Simple.TCP as N
import Servant.API

type Api =
  "spawn"
    :> QueryParam "host" N.HostName
    :> QueryParam "servicename" N.ServiceName
    :>  Get '[JSON] Bool
  :<|>
  "add"
    :> Capture "msg" String
    :> Get '[JSON] Bool

api :: Proxy Api
api = Proxy

