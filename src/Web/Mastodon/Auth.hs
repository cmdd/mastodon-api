{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module : Web.Mastodon.Auth
Description : Code having to do with adding authentication using Servant's generalized auth system
-}
module Web.Mastodon.Auth
  ( Auth
  , MastodonAuth
  , AuthWrapper
  , tokenToAuth
  ) where

import qualified Data.Text          as T
import           GHC.TypeLits       (Symbol)
import           Servant.API        hiding (addHeader)
import           Servant.Client
import           Servant.Common.Req

import           Web.Mastodon.Types

-- | Dummy data type which is used as a tag to AuthProtect so that we can make the type function
--   AuthClientData (which takes an AuthProtect _ type) return the correct authentication
--   method (our MastodonToken).
data MastodonAuth

-- | Adds the necessary @Authorization@ header to the request
--   The way servant\'s generalized authentication works is that by adding the
--   authentication type into each path, each generated client function will
--   now take an @AuthenticateReq (AuthProtect MastodonAuth)@ parameter.
--   The @AuthenticateReq a@ newtype is just a tuple of an @AuthClientData a@ and
--   a function @AuthClientData a -> Req -> Req@
--
--   (@AuthClientData (AuthWrapper MastodonAuth) = MastodonToken@ from the type instance
--   stuff, so in this case @AuthenticateReq (AuthWrapper MastodonAuth)@ is just a tuple
--   of our token and a function which takes a token and an existing request, and modifies
--   the existing request to use the token provided using the function.)
--
--   So when we pass @AuthClientReq (AuthWrapper MastodonAuth)@ to some generated api call,
--   we're passing it our access token and how we want to modify the request to use
--   said access token. In this case, we're adding the @Authorization@ header and
--   putting our token in it.
--
--   @mkAuthorizationReq@ is just a convenience method that curries the @AuthorizationReq@
--   constructor so there aren't tuples everywhere
tokenToAuth :: MastodonToken -> Auth
tokenToAuth key = mkAuthenticateReq key addTokenToReq
  where
    addTokenToReq t = addHeader "Authorization" $ MastodonToken $ T.append "Bearer " $ getMastodonToken t

-- | Convenience type synonym. See documentation on mastodonAuth for details on
--   @AuthenticateReq@.
--
--   Basically we're making a type synonym (\"@Auth@\") for a tuple of the token and how we want
--   to modify a request to take that token
type Auth = AuthenticateReq (AuthProtect MastodonAuth)

-- | Type instance to make @AuthenticateReq@ (which uses this type function)
--   Be a tuple of the token and a function taking a token and existing request
--   and returning a modified request which uses the token
type instance AuthClientData (AuthProtect MastodonAuth) = MastodonToken

-- | Convenience type function which wraps every path with an authentication type
type family AuthWrapper (auth :: *) (api :: *) :: * where
  AuthWrapper auth ((s :: Symbol) :> api) = s :> AuthWrapper auth api
  AuthWrapper auth (l :<|> r) = AuthWrapper auth l :<|> AuthWrapper auth r
  AuthWrapper auth a = auth :> a
