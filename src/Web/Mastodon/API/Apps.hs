{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Apps
Description : Logic dealing with applications (registering an app)

All the requests in this module return ClientM, because we don't have an Auth yet
-}
module Web.Mastodon.API.Apps
  ( Apps
  , appsApi
  , registerApp
  , registerOAuth
  ) where

import           Data.Proxy
import           Servant.API
import           Servant.Client

import           Web.Mastodon.Types

-- This endpoint doesn't need the access token, so no need for AuthWrapper!
type Apps = "apps" :> ReqBody '[FormUrlEncoded] ApplicationPayload :> Post '[JSON] OAuthInfo
       :<|> ".." :> ".." :> "oauth" :> "token" :> ReqBody '[FormUrlEncoded] OAuthPayload :> Post '[JSON] MastodonToken

appsApi :: Proxy Apps
appsApi = Proxy

registerApp   :: ApplicationPayload -> ClientM OAuthInfo
registerOAuth :: OAuthPayload -> ClientM MastodonToken
registerApp :<|> registerOAuth = client appsApi
