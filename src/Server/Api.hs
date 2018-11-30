{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Api where

import Data.Proxy
import Servant.API

type Api =
  "add" :> Capture "msg" String :> Get '[JSON] Bool

api :: Proxy Api
api = Proxy

