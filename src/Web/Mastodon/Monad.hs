{-|
Module : Web.Mastodon.Monad
Description : Provides a configuration data type and a Mastodon reader monad which uses it -}
module Web.Mastodon.Monad
  ( Mastodon
  , runMastodon
  ) where

import           Control.Monad.Trans.Reader
import           Servant.Client             (ClientEnv, ClientM, ServantError,
                                             runClientM)

import           Web.Mastodon.Types         (MastodonToken(..))

-- | Reader monad representing a Mastodon computation which takes a Mastodon configuration
type Mastodon = ReaderT MastodonToken ClientM

-- | \"run\" a Mastodon computation.
--   This is more convenient than a runReaderT since you don't have to runClientM (runReaderT);
--   everything is flattened out.
runMastodon :: Mastodon a -> MastodonToken -> ClientEnv -> IO (Either ServantError a)
runMastodon m t = runClientM (runReaderT m t)
