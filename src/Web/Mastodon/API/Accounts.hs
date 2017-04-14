{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.API.Accounts
Description : Logic for generating API stuff related to accounts (based on arbitrary id)
-}
module Web.Mastodon.API.Accounts
  ( Accounts
  , accountM
  , verifyCredentialsM
  , followersM
  , followingM
  , statusesM
  , followM
  , unfollowM
  , blockM
  , unblockM
  , muteM
  , unmuteM
  , relationshipsM
  , searchM
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
accountM :: Uid Account -> Mastodon Account
accountM aid = ReaderT $ \t ->
  account_ (tokenToAuth t) aid

verifyCredentialsM :: Mastodon Account
verifyCredentialsM = ReaderT $ \t ->
  verifyCredentials_ $ tokenToAuth t

followersM :: Uid Account -> Mastodon [Account]
followersM aid = ReaderT $ \t ->
  followers_ (tokenToAuth t) aid

followingM :: Uid Account -> Mastodon [Account]
followingM aid = ReaderT $ \t ->
  following_ (tokenToAuth t) aid

statusesM :: Uid Account -> Bool -> Bool -> Mastodon [Status]
statusesM aid onlyMedia noReplies = ReaderT $ \t ->
  statuses_ (tokenToAuth t) aid onlyMedia noReplies

followM :: Uid Account -> Mastodon Account
followM aid = ReaderT $ \t ->
  follow_ (tokenToAuth t) aid

unfollowM :: Uid Account -> Mastodon Account
unfollowM aid = ReaderT $ \t ->
  unfollow_ (tokenToAuth t) aid

blockM :: Uid Account -> Mastodon Account
blockM aid = ReaderT $ \t ->
  block_ (tokenToAuth t) aid

unblockM :: Uid Account -> Mastodon Account
unblockM aid = ReaderT $ \t ->
  unblock_ (tokenToAuth t) aid

muteM :: Uid Account -> Mastodon Account
muteM aid = ReaderT $ \t ->
  mute_ (tokenToAuth t) aid

unmuteM :: Uid Account -> Mastodon Account
unmuteM aid = ReaderT $ \t ->
  unmute_ (tokenToAuth t) aid

relationshipsM :: [Uid Account] -> Mastodon [Relationship]
relationshipsM ids = ReaderT $ \t ->
  relationships_ (tokenToAuth t) ids

searchM :: Maybe T.Text -> Maybe Integer -> Mastodon [Account]
searchM q i = ReaderT $ \t ->
  search_ (tokenToAuth t) q i
