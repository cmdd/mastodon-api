{-|
Module : Web.Mastodon.Misc
Description : Miscellaneous (usually simple) helper methods that don't fit anywhere else
-}
module Web.Mastodon.Helpers
  ( getTokenForApp
  , urlGen
  ) where

import qualified Data.Text             as T
import           Servant.Client

import           Web.Mastodon.API.Apps
import           Web.Mastodon.Types

-- | Retrieve a @ClientM MastodonToken@
getTokenForApp :: T.Text -> T.Text -> T.Text -> Maybe T.Text -> T.Text -> T.Text -> ClientM MastodonToken
getTokenForApp appName appRedirUris appScopes appHome email pw = do
  oi <- registerApp $ ApplicationPayload appName appRedirUris appScopes appHome
  registerOAuth $ OAuthPayload oi email pw

-- | Generate a baseUrl from the base domain of an instance, assuming HTTPS and default api path
urlGen :: String -> BaseUrl
urlGen x = BaseUrl Https x 443 "/api/v1"
