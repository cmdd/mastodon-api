{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Timelines
Description : Logic having to do with timelines
-}
module Web.Mastodon.API.Timelines
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

-- QueryParam a Bool and not QueryFlag a because this is optional
type TimelinesInsecure = "timelines" :> "home" :> QueryParam "local" Bool :> Get '[JSON] [Status]
                    :<|> "timelines" :> "public" :> QueryParam "local" Bool :> Get '[JSON] [Status]
                    :<|> "timelines" :> "tag" :> Capture "hashtag" T.Text :> Get '[JSON] [Status]

type Timelines = AuthWrapper (AuthProtect MastodonAuth) TimelinesInsecure

timelinesApi :: Proxy Timelines
timelinesApi = Proxy