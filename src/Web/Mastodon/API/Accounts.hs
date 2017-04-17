{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Accounts
Description : Logic for generating API stuff related to accounts (based on arbitrary id)
-}
module Web.Mastodon.API.Accounts
  ( Accounts
  , accountsApi
  , getAccountByUid
  , verifyCredentials
  , getFollowersForUid
  , getFollowingForUid
  , getStatusesForUid
  , followUid
  , unfollowUid
  , blockUid
  , unblockUid
  , muteUid
  , unmuteUid
  , getRelationships
  , searchAccounts
  ) where

import           Control.Monad.Trans.Reader
import           Data.Proxy
import qualified Data.Text                  as T
import           Servant.API
import           Servant.Client

import           Web.Mastodon.Auth
import           Web.Mastodon.Monad
import           Web.Mastodon.Types

-- TODO: Re-export Auth (and maybe types) to avoid others having to import it when importing api?

-- TODO: Int instead of Integer?
type AccountsInsecure = "accounts" :> Capture "id" (Uid Account) :> Get '[JSON] Account
                   :<|> "accounts" :> "verify_credentials" :> Get '[JSON] Account
                   :<|> "accounts" :> Capture "id" (Uid Account) :> "followers" :> Get '[JSON] [Account]
                   :<|> "accounts" :> Capture "id" (Uid Account) :> "following" :> Get '[JSON] [Account]
                   :<|> "accounts" :> Capture "id" (Uid Account) :> "statuses" :> QueryFlag "only_media" :> QueryFlag "exclude_replies" :> Get '[JSON] [Status]
                   :<|> "accounts" :> Capture "id" (Uid Account) :> "follow" :> Get '[JSON] Account
                   :<|> "accounts" :> Capture "id" (Uid Account) :> "unfollow" :> Get '[JSON] Account
                   :<|> "accounts" :> Capture "id" (Uid Account) :> "block" :> Get '[JSON] Account
                   :<|> "accounts" :> Capture "id" (Uid Account) :> "unblock" :> Get '[JSON] Account
                   :<|> "accounts" :> Capture "id" (Uid Account) :> "mute" :> Get '[JSON] Account
                   :<|> "accounts" :> Capture "id" (Uid Account) :> "unmute" :> Get '[JSON] Account
                   :<|> "accounts" :> "relationships" :> QueryParams "id" (Uid Account) :> Get '[JSON] [Relationship]
                   :<|> "accounts" :> "search" :> QueryParam "q" T.Text :> QueryParam "limit" Integer :> Get '[JSON] [Account]

-- | Secure Accounts API type
type Accounts = AuthWrapper (AuthProtect MastodonAuth) AccountsInsecure

accountsApi :: Proxy Accounts
accountsApi = Proxy

account_           :: Auth -> Uid Account -> ClientM Account
verifyCredentials_ :: Auth -> ClientM Account
followers_         :: Auth -> Uid Account -> ClientM [Account]
following_         :: Auth -> Uid Account -> ClientM [Account]
statuses_          :: Auth -> Uid Account -> Bool -> Bool -> ClientM [Status]
follow_            :: Auth -> Uid Account -> ClientM Account
unfollow_          :: Auth -> Uid Account -> ClientM Account
block_             :: Auth -> Uid Account -> ClientM Account
unblock_           :: Auth -> Uid Account -> ClientM Account
mute_              :: Auth -> Uid Account -> ClientM Account
unmute_            :: Auth -> Uid Account -> ClientM Account
relationships_     :: Auth -> [Uid Account] -> ClientM [Relationship]
search_            :: Auth -> Maybe T.Text -> Maybe Integer -> ClientM [Account]

account_
  :<|> verifyCredentials_
  :<|> followers_
  :<|> following_
  :<|> statuses_
  :<|> follow_
  :<|> unfollow_
  :<|> block_
  :<|> unblock_
  :<|> mute_
  :<|> unmute_
  :<|> relationships_
  :<|> search_
  = client accountsApi

-- | Retrieve an @Account@ from an account id.
getAccountByUid :: Uid Account -> Mastodon Account
getAccountByUid aid = ReaderT $ \t -> account_ (tokenToAuth t) aid

verifyCredentials :: Mastodon Account
verifyCredentials = ReaderT $ \t -> verifyCredentials_ $ tokenToAuth t

getFollowersForUid :: Uid Account -> Mastodon [Account]
getFollowersForUid aid = ReaderT $ \t -> followers_ (tokenToAuth t) aid

getFollowingForUid :: Uid Account -> Mastodon [Account]
getFollowingForUid aid = ReaderT $ \t -> following_ (tokenToAuth t) aid

getStatusesForUid :: Uid Account -> Bool -> Bool -> Mastodon [Status]
getStatusesForUid aid onlyMedia noReplies = ReaderT $ \t -> statuses_ (tokenToAuth t) aid onlyMedia noReplies

followUid :: Uid Account -> Mastodon Account
followUid tid = ReaderT $ \t -> follow_ (tokenToAuth t) tid

unfollowUid :: Uid Account -> Mastodon Account
unfollowUid tid = ReaderT $ \t -> unfollow_ (tokenToAuth t) tid

blockUid :: Uid Account -> Mastodon Account
blockUid tid = ReaderT $ \t -> block_ (tokenToAuth t) tid

unblockUid :: Uid Account -> Mastodon Account
unblockUid tid = ReaderT $ \t -> unblock_ (tokenToAuth t) tid

muteUid :: Uid Account -> Mastodon Account
muteUid tid = ReaderT $ \t -> mute_ (tokenToAuth t) tid

unmuteUid :: Uid Account -> Mastodon Account
unmuteUid tid = ReaderT $ \t -> unmute_ (tokenToAuth t) tid

getRelationships :: [Uid Account] -> Mastodon [Relationship]
getRelationships tids = ReaderT $ \t -> relationships_ (tokenToAuth t) tids

searchAccounts :: Maybe T.Text -> Maybe Integer -> Mastodon [Account]
searchAccounts q i = ReaderT $ \t -> search_ (tokenToAuth t) q i
