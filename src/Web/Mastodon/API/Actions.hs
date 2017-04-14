{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Actions
Description : API calls which act using the verified (authenticated) user
-}
module Web.Mastodon.API.Actions
  ( Actions
  , actionsApi
  , getBlocks
  , getFavorites
  , getFollowRequests
  , authorizeFollowRequest
  , rejectFollowRequest
  , followUri
  , getInstance
  , getMutes
  , getNotifications
  , getNotificationById
  , clearNotifications
  , getReports
  , search
  ) where

import           Control.Monad.Trans.Reader
import           Data.Proxy
import           Servant.API
import           Servant.Client

import           Web.Mastodon.Auth
import           Web.Mastodon.Monad
import           Web.Mastodon.Types

-- TODO: This does not include media endpoint, which is for file uploading
type ActionsInsecure = "blocks" :> Get '[JSON] [Account]
                  :<|> "favourites" :> Get '[JSON] [Status]
                  :<|> "follow_requests" :> Get '[JSON] [Account]
                  :<|> "follow_requests" :> "authorize" :> ReqBody '[FormUrlEncoded] (UidPayload Account) :> Post '[JSON] NoContent
                  :<|> "follow_requests" :> "reject" :> ReqBody '[FormUrlEncoded] (UidPayload Account) :> Post '[JSON] NoContent
                  :<|> "follows" :> ReqBody '[FormUrlEncoded] UriPayload :> Post '[JSON] Account
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

blocks_                 :: Auth -> ClientM [Account]
favorites_              :: Auth -> ClientM [Status]
followRequests_         :: Auth -> ClientM [Account]
authorizeFollowRequest_ :: Auth -> UidPayload Account -> ClientM NoContent
rejectFollowRequest_    :: Auth -> UidPayload Account -> ClientM NoContent
follows_                :: Auth -> UriPayload -> ClientM Account
instance_               :: Auth -> ClientM Account
mutes_                  :: Auth -> ClientM [Account]
notifications_          :: Auth -> ClientM [Notification]
notificationByid_       :: Auth -> Uid Notification -> ClientM Notification
clearNotifications_     :: Auth -> ClientM NoContent
reports_                :: Auth -> ClientM [Report]
search_                 :: Auth -> SearchQuery -> ClientM SearchResults

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

getBlocks :: Mastodon [Account]
getBlocks = ReaderT $ \t -> blocks_ (tokenToAuth t)

getFavorites :: Mastodon [Status]
getFavorites = ReaderT $ \t -> favorites_ (tokenToAuth t)

getFollowRequests :: Mastodon [Account]
getFollowRequests = ReaderT $ \t -> followRequests_ (tokenToAuth t)

authorizeFollowRequest :: UidPayload Account -> Mastodon NoContent
authorizeFollowRequest uo = ReaderT $ \t -> authorizeFollowRequest_ (tokenToAuth t) uo

rejectFollowRequest :: UidPayload Account -> Mastodon NoContent
rejectFollowRequest uo = ReaderT $ \t -> rejectFollowRequest_ (tokenToAuth t) uo

followUri :: UriPayload -> Mastodon Account
followUri uo = ReaderT $ \t -> follows_ (tokenToAuth t) uo

getInstance :: Mastodon Account
getInstance = ReaderT $ \t -> instance_ (tokenToAuth t)

getMutes :: Mastodon [Account]
getMutes = ReaderT $ \t -> mutes_ (tokenToAuth t)

getNotifications :: Mastodon [Notification]
getNotifications = ReaderT $ \t -> notifications_ (tokenToAuth t)

getNotificationById :: Uid Notification -> Mastodon Notification
getNotificationById nid = ReaderT $ \t -> notificationByid_ (tokenToAuth t) nid

clearNotifications :: Mastodon NoContent
clearNotifications = ReaderT $ \t -> clearNotifications_ (tokenToAuth t)

getReports :: Mastodon [Report]
getReports = ReaderT $ \t -> reports_ (tokenToAuth t)

search :: SearchQuery -> Mastodon SearchResults
search q = ReaderT $ \t -> search_ (tokenToAuth t) q