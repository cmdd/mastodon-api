{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Apps
Description : Logic dealing with applications (registering an app)
-}
module Web.Mastodon.API.Apps
  (
  ) where

import Data.Proxy
import Servant.API
import Servant.Client

import Web.Mastodon.Types

-- This endpoint doesn't need the access token, so no need for AuthWrapper!
-- type Apps = "apps" :> ReqBody '[FormUrlEncoded] ApplicationInfo :> Post OAuthInfo
-- 
-- appsApi :: Proxy Apps
-- appsApi = Proxy