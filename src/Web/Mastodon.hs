{-|
Module : Web.Mastodon
Description : Top-level module which re-exports everything important
-}
module Web.Mastodon
    ( module MAPI
    ) where

import           Web.Mastodon.API     as MAPI
import           Web.Mastodon.Helpers as MAPI
import           Web.Mastodon.Monad   as MAPI
import           Web.Mastodon.Types   as MAPI
