{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Statuses
Description : Logic having to do with statuses (by id)
-}
module Web.Mastodon.API.Statuses
  ( Statuses
  , statusesApi
  , getStatus
  , getStatusContext
  , getStatusCard
  , getStatusRebloggedBy
  , getStatusFavoritedBy
  , postStatus
  , deleteStatus
  , reblogStatus
  , unreblogStatus
  , favoriteStatus
  , unfavoriteStatus
  ) where

import           Control.Monad.Trans.Reader
import           Data.Proxy
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

status_           :: Auth -> Uid Status -> ClientM Status
context_          :: Auth -> Uid Status -> ClientM Context
card_             :: Auth -> Uid Status -> ClientM Card
rebloggedBy_      :: Auth -> Uid Status -> ClientM [Account]
favoritedBy_      :: Auth -> Uid Status -> ClientM [Account]
postStatus_       :: Auth -> StatusPayload -> ClientM Status
deleteStatus_     :: Auth -> Uid Status -> ClientM NoContent
reblogStatus_     :: Auth -> Uid Status -> ClientM Status
unreblogStatus_   :: Auth -> Uid Status -> ClientM Status
favoriteStatus_   :: Auth -> Uid Status -> ClientM Status
unfavoriteStatus_ :: Auth -> Uid Status -> ClientM Status

status_
  :<|> context_
  :<|> card_
  :<|> rebloggedBy_
  :<|> favoritedBy_
  :<|> postStatus_
  :<|> deleteStatus_
  :<|> reblogStatus_
  :<|> unreblogStatus_
  :<|> favoriteStatus_
  :<|> unfavoriteStatus_
  = client statusesApi

getStatus :: Uid Status -> Mastodon Status
getStatus sid = ReaderT $ \t -> status_ (tokenToAuth t) sid

getStatusContext :: Uid Status -> Mastodon Context
getStatusContext sid = ReaderT $ \t -> context_ (tokenToAuth t) sid

getStatusCard :: Uid Status -> Mastodon Card
getStatusCard sid = ReaderT $ \t -> card_ (tokenToAuth t) sid

getStatusRebloggedBy :: Uid Status -> Mastodon [Account]
getStatusRebloggedBy sid = ReaderT $ \t -> rebloggedBy_ (tokenToAuth t) sid

getStatusFavoritedBy :: Uid Status -> Mastodon [Account]
getStatusFavoritedBy sid = ReaderT $ \t -> favoritedBy_ (tokenToAuth t) sid

postStatus :: StatusPayload -> Mastodon Status
postStatus s = ReaderT $ \t -> postStatus_ (tokenToAuth t) s

deleteStatus :: Uid Status -> Mastodon NoContent
deleteStatus sid = ReaderT $ \t -> deleteStatus_ (tokenToAuth t) sid

reblogStatus :: Uid Status -> Mastodon Status
reblogStatus sid = ReaderT $ \t -> reblogStatus_ (tokenToAuth t) sid

unreblogStatus :: Uid Status -> Mastodon Status
unreblogStatus sid = ReaderT $ \t -> unreblogStatus_ (tokenToAuth t) sid

favoriteStatus :: Uid Status -> Mastodon Status
favoriteStatus sid = ReaderT $ \t -> favoriteStatus_ (tokenToAuth t) sid

unfavoriteStatus :: Uid Status -> Mastodon Status
unfavoriteStatus sid = ReaderT $ \t -> unfavoriteStatus_ (tokenToAuth t) sid