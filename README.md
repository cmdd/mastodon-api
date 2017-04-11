# mastodon-api

A Haskell interface to the [Mastodon](https://github.com/tootsuite/mastodon) API.

## Usage example
```haskell
-- Overloaded strings because MastodonToken takes a Text type
{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Servant.Client
import           Web.Mastodon.API        as API
import           Web.Mastodon.Auth
import           Web.Mastodon.Monad

run :: IO ()
run = do
  manager <- newManager tlsManagerSettings
  let token = MastodonToken "your bearer token here"
  let clientEnv = ClientEnv manager (BaseUrl Https "social.tchncs.de" 443 "/api/v1")
  res <- runMastodon (do
    relationshipsM [20232]
    ) token clientEnv

  case res of
    Left err  -> putStrLn $ "Error: " ++ show err
    Right acc -> print acc
```
