{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
    ( main
    ) where

import           Data.Either             (isRight)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative
import           System.Environment      (withArgs)
import           Servant.Client
import           Test.Hspec
import qualified Data.Text               as T

import           Web.Mastodon.API        as API
import           Web.Mastodon.Auth
import           Web.Mastodon.Monad
import           Web.Mastodon.Types

data Options = Options
  { optToken     :: String
  , optBaseUrl   :: String
  , optHSpecOpts :: Maybe [String]
  } deriving (Show)

-- TODO: Once Apps.hs is implemented, this shouldn't be necessary
options :: Parser Options
options = Options
    <$> strOption
        ( long "token"
       <> short 't'
       <> metavar "BEARER_TOKEN"
       <> help "Bearer token (aka access token) acquired through OAuth reg")
    <*> strOption
        ( long "base-url"
       <> short 'b'
       <> metavar "BASE_URL"
       <> value "social.tchncs.de"
       <> help "The base url of the Mastodon instance")
    <*> optional (some (argument str
        ( metavar "HSPEC_ARGS"
       <> help "Hspec arguments")))

-- TODO: main
main :: IO ()
main = do
  Options{..} <- execParser opts

  let hSpecArgs = fromMaybe [] optHSpecOpts
      base = optBaseUrl
      token = MastodonToken $ T.pack optToken

  withArgs hSpecArgs $ hspec (runSpec token base)

  where
    opts = info (helper <*> options)
         ( fullDesc
        <> progDesc "Run the mastodon-api tests")

runSpec :: MastodonToken -> String -> Spec
runSpec token base =
  describe "Web.Mastodon.API" $
    it "retrieves an authenticated account" $
      getId token base `shouldReturn` True

-- Copied straight from the README
getId :: MastodonToken -> String -> IO Bool
getId token base = do
  manager <- newManager tlsManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Https base 443 "/api/v1")
  res <- runMastodon verifyCredentials token clientEnv
  print res
  return $ isRight res
