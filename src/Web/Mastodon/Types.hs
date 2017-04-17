{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Web.Mastodon.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.HashMap.Lazy   (fromList)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           Lens.Micro.TH
import           Web.FormUrlEncoded
import           Web.HttpApiData     (ToHttpApiData, toQueryParam)

-- TODO: Int instead of Integer? (using Integer can cause overflow)
-- TODO: Proper time & url parsing

-- maybeParse :: MonadPlus m => (a -> Maybe b) -> m a -> m b
-- maybeParse f = (maybe mzero return . f =<<)
--
-- fromURI :: MonadPlus m => m String -> m URI
-- fromURI = maybeParse URI.parseURI

-- | Represents a unique id number for some type.
--   Rather than making a discrete @Uid@ type for every data type (@AccUid@, @NotifUid@, etc.),
--   we make a newtype with a phantom type parameter to distinguish an account id (@Uid Account@)
--   from a notification id (@Uid Notification@) while making them both Integers under the hood
--
--   In case we don't want to use an @Integer@ to represent Uids (for memory overflow reasons),
--   we can also change every id type from right here.
--
--   Plus, if we try to pass a Uid Notification to a Uid Account, we'll type error
--   (as we should).
--
--   Since we're using @GeneralizedNewtypeDeriving@ and since we have a @FromJSON@
--   instance in our deriving clause, this will be treated like an @Integer@.
newtype Uid a = Uid { getUid :: Integer } deriving (Eq, Show, FromJSON, ToJSON, ToHttpApiData)

-- TODO: type Count = Integer ?

-- Sum types for fields with choice
data AttachmentType = Image | Video | Gifv deriving (Show)

instance FromJSON AttachmentType where
  parseJSON = withText "attachment type" $ \t ->
    case t of
      "image" -> pure Image
      "video" -> pure Video
      "gifv"  -> pure Gifv
      _       -> empty

data NotificationType = MentionNotif | ReblogNotif | FavoriteNotif | FollowNotif deriving (Show)

instance FromJSON NotificationType where
  parseJSON = withText "notification type" $ \t ->
    case t of
      "mention"   -> pure MentionNotif
      "reblog"    -> pure ReblogNotif
      "favourite" -> pure FavoriteNotif
      "follow"    -> pure FollowNotif
      _           -> empty

data Visibility = Public | Unlisted | Private | Direct deriving (Show)

instance FromJSON Visibility where
  parseJSON = withText "visibility" $ \t ->
    case t of
      "public"   -> pure Public
      "unlisted" -> pure Unlisted
      "private"  -> pure Private
      "direct"   -> pure Direct
      _          -> empty

instance ToHttpApiData Visibility where
  toQueryParam Public   = toQueryParam ("public"   :: T.Text)
  toQueryParam Unlisted = toQueryParam ("unlisted" :: T.Text)
  toQueryParam Private  = toQueryParam ("private"  :: T.Text)
  toQueryParam Direct   = toQueryParam ("direct"   :: T.Text)

-- Datatypes

data Account = Account
  { _accountUid            :: Uid Account
  , _accountUsername       :: T.Text
  , _accountAcct           :: T.Text
  , _accountDisplayName    :: T.Text
  , _accountBio            :: T.Text
  , _accountUrl            :: Url
  , _accountAvatar         :: Url
  , _accountHeader         :: Url
  , _accountLocked         :: Bool
  , _accountCreatedAt      :: T.Text
  , _accountFollowersCount :: Integer
  , _accountFollowingCount :: Integer
  , _accountStatusesCount  :: Integer
  } deriving (Show)

instance FromJSON Account where
  parseJSON = withObject "account" $ \o ->
    Account <$> o .: "id"
            <*> o .: "username"
            <*> o .: "acct"
            <*> o .: "display_name"
            <*> o .: "note"
            <*> o .: "url"
            <*> o .: "avatar"
            <*> o .: "header"
            <*> o .: "locked"
            <*> o .: "created_at"
            <*> o .: "followers_count"
            <*> o .: "following_count"
            <*> o .: "statuses_count"

data Application = Application
  { _applicationName    :: T.Text
  , _applicationWebsite :: T.Text
  } deriving (Show)

instance FromJSON Application where
  parseJSON = withObject "application" $ \o ->
    Application <$> o .: "name"
                <*> o .: "website"

data Attachment = Attachment
  { _attachmentUid        :: Uid Attachment
  , _attachmentTypeOf     :: AttachmentType
  , _attachmentUrl        :: Url
  , _attachmentRemoteUrl  :: Maybe Url
  , _attachmentPreviewUrl :: Url
  , _attachmentTextUrl    :: Maybe Url
  } deriving (Show)

instance FromJSON Attachment where
  parseJSON = withObject "attachment" $ \o ->
    Attachment <$> o .:  "id"
               <*> o .:  "type"
               <*> o .:  "url"
               <*> o .:? "remote_url"
               <*> o .:  "preview_url"
               <*> o .:? "text_url"

data Card = Card
  { _cardUrl         :: Url
  , _cardTitle       :: T.Text
  , _cardDescription :: T.Text
  , _cardImage       :: Maybe Url
  } deriving (Show)

instance FromJSON Card where
  parseJSON = withObject "card" $ \o ->
    Card <$> o .:  "url"
         <*> o .:  "title"
         <*> o .:  "description"
         <*> o .:? "image"

data Context = Context
  { _contextAncestors   :: [Status]
  , _contextDescendants :: [Status]
  } deriving (Show)

instance FromJSON Context where
  parseJSON = withObject "context" $ \o ->
    Context <$> o .: "ancestors"
            <*> o .: "descendants"

data Notification = Notification
  { _notificationUid       :: Uid Notification
  , _notificationTypeOf    :: NotificationType
  , _notificationCreatedAt :: Time
  , _notificationAccount   :: Account
  , _notificationStatus    :: Maybe Status
  } deriving (Show)

instance FromJSON Notification where
  parseJSON = withObject "notification" $ \o ->
    Notification <$> o .:  "id"
                 <*> o .:  "type"
                 <*> o .:  "created_at"
                 <*> o .:  "account"
                 <*> o .:? "status"

-- | Newtype for the Mastodon access token
newtype MastodonToken = MastodonToken { getMastodonToken :: T.Text } deriving (Show, ToHttpApiData)

instance FromJSON MastodonToken where
  parseJSON = withObject "mastodon token" $ \o ->
    MastodonToken <$> o .: "access_token"

data Mention = Mention
  { _mentionUrl      :: Url
  , _mentionUsername :: T.Text
  , _mentionAcct     :: T.Text
  , _mentionUid      :: Uid Account
  } deriving (Show)

instance FromJSON Mention where
  parseJSON = withObject "mention" $ \o ->
    Mention <$> o .: "url"
            <*> o .: "username"
            <*> o .: "acct"
            <*> o .: "id"

-- | Type representing OAuth info for an app.
--   The client id field is not uid since this isn't an integer
data OAuthInfo = OAuthInfo
  { _oAuthInfoUid          :: Uid Application
  , _oAuthInfoClientId     :: T.Text
  , _oAuthInfoClientSecret :: T.Text
  } deriving (Show)

instance FromJSON OAuthInfo where
  parseJSON = withObject "oauthinfo" $ \o ->
    OAuthInfo <$> o .: "id"
              <*> o .: "client_id"
              <*> o .: "client_secret"

data Relationship = Relationship
  { _relationshipFollowing  :: Bool
  , _relationshipFollowedBy :: Bool
  , _relationshipBlocking   :: Bool
  , _relationshipMuting     :: Bool
  , _relationshipRequested  :: Bool
  } deriving (Show)

instance FromJSON Relationship where
  parseJSON = withObject "relationship" $ \o ->
    Relationship <$> o .: "following"
                 <*> o .: "followed_by"
                 <*> o .: "blocking"
                 <*> o .: "muting"
                 <*> o .: "requested"

data Report = Report
  { reportUid    :: Uid Report
  , reportAction :: T.Text
  } deriving (Show)

instance FromJSON Report where
  parseJSON = withObject "report" $ \o ->
    Report <$> o .: "id"
           <*> o .: "action_taken"


data SearchResults = SearchResults
  { _searchResultsAccounts :: [Account]
  , _searchResultsStatuses :: [Status]
  , _searchResultsHashtags :: [T.Text]
  } deriving (Show)

instance FromJSON SearchResults where
  parseJSON = withObject "search results" $ \o ->
    SearchResults <$> o .: "accounts"
                  <*> o .: "statuses"
                  <*> o .: "hashtags"

-- TODO: What is type of uri?
data Status = Status
  { _statusUid                 :: Uid Status
  , _statusUri                 :: T.Text
  , _statusUrl                 :: Url
  , _statusAccount             :: Account
  , _statusInReplyToUid        :: Maybe (Uid Status)
  , _statusInReplyToAccountUid :: Maybe (Uid Account)
  , _statusReblog              :: Maybe Status
  , _statusContent             :: T.Text
  , _statusCreatedAt           :: Time
  , _statusReblogsCount        :: Integer
  , _statusFavoritesCount      :: Integer
  , _statusReblogged           :: Bool
  , _statusFavorited           :: Bool
  , _statusSensitive           :: Bool
  , _statusSpoilerText         :: T.Text
  , _statusVisibility          :: Visibility
  , _statusMediaAttachments    :: [Attachment]
  , _statusMentions            :: [Mention]
  , _statusTags                :: [Tag]
  , _statusApplication         :: Application
  } deriving (Show)

instance FromJSON Status where
  parseJSON = withObject "status" $ \o ->
    Status <$> o .:  "id"
           <*> o .:  "uri"
           <*> o .:  "url"
           <*> o .:  "account"
           <*> o .:? "in_reply_to_id"
           <*> o .:? "in_reply_to_account_id"
           <*> o .:? "reblog"
           <*> o .:  "content"
           <*> o .:  "created_at"
           <*> o .:  "reblogs_count"
           <*> o .:  "favourites_count"
           <*> o .:  "reblogged"
           <*> o .:  "favourited"
           <*> o .:  "sensitive"
           <*> o .:  "spoiler_text"
           <*> o .:  "visibility"
           <*> o .:  "media_attachments"
           <*> o .:  "mentions"
           <*> o .:  "tags"
           <*> o .:  "application"

data Tag = Tag
  { _tagName :: T.Text
  , _tagUrl  :: Url
  } deriving (Show)

instance FromJSON Tag where
  parseJSON = withObject "tag" $ \o ->
    Tag <$> o .: "name"
        <*> o .: "url"

-- TODO: An actual Time type, not just a string
newtype Time = Time { getTime :: T.Text } deriving (Show)

instance FromJSON Time where
  parseJSON = withText "time" $ pure . Time

-- TODO: An actual Url type, not just a string
newtype Url = Url { getUrl :: T.Text } deriving (Show)

instance FromJSON Url where
  parseJSON = withText "url" $ pure. Url

-- Types for only sending things

-- | Type containing necessary fields to register for an application to get client id and secret
data ApplicationPayload = ApplicationPayload
  { _applicationPayloadName         :: T.Text
  , _applicationPayloadRedirectUris :: T.Text
  , _applicationPayloadScopes       :: T.Text
  , _applicationPayloadWebsite      :: Maybe T.Text
  } deriving (Show)

instance ToForm ApplicationPayload where
  toForm (ApplicationPayload name uris scopes site) =
    [ ("client_name",   toQueryParam name)
    , ("redirect_uris", toQueryParam uris)
    , ("scopes",        toQueryParam scopes)
    , ("website",       toQueryParam site) ]

-- | Type for information needed to register with OAuth.
data OAuthPayload = OAuthPayload
  { _oAuthPayloadInfo     :: OAuthInfo
  , _oAuthPayloadEmail    :: T.Text
  , _oAuthPayloadPassword :: T.Text
  } deriving (Show)

instance ToForm OAuthPayload where
  toForm (OAuthPayload (OAuthInfo _ cid cs) e p) =
    [ ("client_id",     toQueryParam cid)
    , ("client_secret", toQueryParam cs)
    , ("grant_type",    toQueryParam ("password" :: T.Text))
    , ("username",      toQueryParam e)
    , ("password",      toQueryParam p) ]

data SearchQuery = SearchQuery
  { _searchQueryQuery   :: T.Text
  , _searchQueryResolve :: Bool
  } deriving (Show)

instance ToForm SearchQuery where
  toForm (SearchQuery q r) =
    [ ("q",       toQueryParam q)
    , ("resolve", toQueryParam r) ]

data StatusPayload = StatusPayload
  { _statusPayloadStatus       :: T.Text
  , _statusPayloadInReplyToUid :: Maybe (Uid Status)
  , _statusPayloadMediaUids    :: Maybe [Uid Attachment]
  , _statusPayloadSensitive    :: Maybe Bool
  , _statusPayloadSpoilerText  :: Maybe T.Text
  , _statusPayloadVisibility   :: Maybe Visibility
  } deriving (Show)

instance ToForm StatusPayload where
  toForm (StatusPayload status rid mids sens st vis) =
    [ ("status",         toQueryParam status)
    , ("in_reply_to_id", toQueryParam rid)
    , ("sensitive",      toQueryParam sens)
    , ("spoiler_text",   toQueryParam st)
    , ("visibility",     toQueryParam vis) ] <> hmap
    where
      keys = map (\y -> "media_ids" <> "[" <> y <> "]") (fmap (T.pack . show) [0..])
      hash = zip keys $ toQueryParam <$> sequenceA mids
      hmap = Form . fromList . fmap (\(k, v) -> (k, [v])) $ hash

-- | Type which represents a form payload of some Uid.
--   Essentially, an object with just an id field.
newtype UidPayload a = UidPayload (Uid a) deriving (Show)

instance ToForm (UidPayload a) where
  toForm (UidPayload a) = [("id", toQueryParam a)]

newtype UriPayload = UriPayload T.Text deriving (Show)

instance ToForm UriPayload where
  toForm (UriPayload a) = [("uri", toQueryParam a)]

-- Lenses
makeFields ''Account
makeFields ''Application
-- makeFields ''ApplicationPayload
makeFields ''Attachment
makeFields ''Card
makeFields ''Context
makeFields ''Notification
makeFields ''Mention
makeFields ''OAuthInfo
-- makeFields ''OAuthPayload
makeFields ''Relationship
makeFields ''Report
-- makeFields ''SearchQuery
makeFields ''SearchResults
makeFields ''Status
-- makeFields ''StatusPayload
makeFields ''Tag
