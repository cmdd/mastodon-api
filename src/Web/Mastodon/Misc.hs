{-|
Module : Web.Mastodon.Misc
Description : Miscellaneous (usually simple) helper methods that don't fit anywhere else
-}
module Web.Mastodon.Misc
  (
  ) where

import           Servant.Client (BaseUrl (..), Scheme (..))

-- | Generate a baseUrl from the base domain of an instance, assuming HTTPS and default api path
urlGen :: String -> BaseUrl
urlGen x = BaseUrl Https x 443 "/api/v1"
