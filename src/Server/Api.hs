{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Api where

import Data.Proxy
import Servant.API
import Data.Text (Text)

type Api =
  "add" :> Capture "msg" Text :> Get '[JSON] Bool

api :: Proxy Api
api = Proxy

