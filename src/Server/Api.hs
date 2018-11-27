{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Api where

import Data.Proxy
import Data.Text
import Servant.API

type Api =
  "add" :> Capture "msg" Text :> Get '[JSON] Bool

api :: Proxy Api
api = Proxy

type DocsAPI = Api :<|> Raw

docsApi :: Proxy DocsAPI
docsApi = Proxy


