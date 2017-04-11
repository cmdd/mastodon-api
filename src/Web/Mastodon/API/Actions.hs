{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Actions
Description : API calls which act using the verified (authenticated) user
-}
module Web.Mastodon.API.Actions
  (
  ) where

import           Data.Proxy
import qualified Data.Text          as T
import           Servant.API
import           Servant.Client

import           Web.Mastodon.Auth
import           Web.Mastodon.Monad
import           Web.Mastodon.Types

-- TODO: This does not include media endpoint, which is for file uploading
type ActionsInsecure = "blocks" :> Get '[JSON] [Account]
                  :<|> "favourites" :> Get '[JSON] [Status]
                  :<|> "follow_requests" :> Get '[JSON] [Account]
                  :<|> "follow_requests" :> "authorize" :> ReqBody '[FormUrlEncoded] (UidObj Account) :> Post '[JSON] NoContent
                  :<|> "follow_requests" :> "reject" :> ReqBody '[FormUrlEncoded] (UidObj Account) :> Post '[JSON] NoContent
                  :<|> "follows" :> ReqBody '[FormUrlEncoded] UriObj :> Post '[JSON] Account
                  :<|> "instance" :> Get '[JSON] Account
                  :<|> "mutes" :> Get '[JSON] [Account]
                  :<|> "notifications" :> Get '[JSON] [Notification]
                  :<|> "notifications" :> Capture "id" (Uid Notification) :> Get '[JSON] Notification
                  :<|> "notifications" :> "clear" :> Post '[JSON] NoContent
                  :<|> "reports" :> Get '[JSON] [Report]
                  :<|> "search" :> ReqBody '[FormUrlEncoded] SearchQuery :> Get '[JSON] SearchResults

type Actions = AuthWrapper (AuthProtect MastodonAuth) ActionsInsecure

actionsApi :: Proxy Actions
actionsApi = Proxy

blocks_
  :<|> favorites_
  :<|> followRequests_
  :<|> authorizeFollowRequest_
  :<|> rejectFollowRequest_
  :<|> follows_
  :<|> instance_
  :<|> mutes_
  :<|> notifications_
  :<|> notificationByid_
  :<|> clearNotifications_
  :<|> reports_
  :<|> search_
  = client actionsApi

-- TODO
