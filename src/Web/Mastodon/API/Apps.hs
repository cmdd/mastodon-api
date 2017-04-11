{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Apps
Description : Logic dealing with applications (registering an app & obtaining the requisite OAuth stuff/bearer token )-}
module Web.Mastodon.API.Apps
  (
  ) where

import Servant.API
import Servant.Client

-- TODO: Multipart form data?
-- type Apps = "apps" :> ReqBody '[FormUrlEncoded] ApplicationInfo :> Post OAuthInfo
