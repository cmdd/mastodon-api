{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Statuses
Description : Logic having to do with statuses (by id)
-}
module Web.Mastodon.API.Statuses
  (
  ) where

import           Control.Monad.Trans.Reader
import           Data.Proxy
import qualified Data.Text                  as T
import           Servant.API
import           Servant.Client

import           Web.Mastodon.Auth
import           Web.Mastodon.Monad
import           Web.Mastodon.Types

type StatusesInsecure = "statuses" :> Capture "id" (Uid Status) :> Get '[JSON] Status
                   :<|> "statuses" :> Capture "id" (Uid Status) :> "context" :> Get '[JSON] Context
                   :<|> "statuses" :> Capture "id" (Uid Status) :> "card" :> Get '[JSON] Card
                   :<|> "statuses" :> Capture "id" (Uid Status) :> "reblogged_by" :> Get '[JSON] [Account]
                   :<|> "statuses" :> Capture "id" (Uid Status) :> "favourited_By" :> Get '[JSON] [Account]
                   :<|> "statuses" :> ReqBody '[FormUrlEncoded] StatusPayload :> Post '[JSON] Status
                   :<|> "statuses" :> Capture "id" (Uid Status) :> Delete '[JSON] NoContent
                   :<|> "statuses" :> Capture "id" (Uid Status) :> "reblog" :> Post '[JSON] Status
                   :<|> "statuses" :> Capture "id" (Uid Status) :> "unreblog" :> Post '[JSON] Status
                   :<|> "statuses" :> Capture "id" (Uid Status) :> "favourite" :> Post '[JSON] Status
                   :<|> "statuses" :> Capture "id" (Uid Status) :> "unfavourite" :> Post '[JSON] Status

type Statuses = AuthWrapper (AuthProtect MastodonAuth) StatusesInsecure

statusesApi :: Proxy Statuses
statusesApi = Proxy
