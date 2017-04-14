{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Timelines
Description : Logic having to do with timelines
-}
module Web.Mastodon.API.Timelines
  ( Timelines
  , tlHome
  , tlPublic
  , tlTag
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
type TimelinesInsecure = "timelines" :> "home" :> Get '[JSON] [Status]
                    :<|> "timelines" :> "public" :> QueryParam "local" Bool :> Get '[JSON] [Status]
                    :<|> "timelines" :> "tag" :> Capture "hashtag" T.Text :> QueryParam "local" Bool :> Get '[JSON] [Status]

type Timelines = AuthWrapper (AuthProtect MastodonAuth) TimelinesInsecure

timelinesApi :: Proxy Timelines
timelinesApi = Proxy

tlHome_   :: Auth -> ClientM [Status]
tlPublic_ :: Auth -> Maybe Bool -> ClientM [Status]
tlTag_    :: Auth -> T.Text -> Maybe Bool -> ClientM [Status]

tlHome_
  :<|> tlPublic_
  :<|> tlTag_
  = client timelinesApi

tlHome :: Mastodon [Status]
tlHome = ReaderT $ \t -> tlHome_ (tokenToAuth t)

tlPublic :: Maybe Bool -> Mastodon [Status]
tlPublic l = ReaderT $ \t -> tlPublic_ (tokenToAuth t) l

tlTag :: T.Text -> Maybe Bool -> Mastodon [Status]
tlTag hashtag l = ReaderT $ \t -> tlTag_ (tokenToAuth t) hashtag l