module Slack.Type where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text
import Prelude

data MessageResponse = MessageResponse
    { msgResponceTimestamp :: Text
    , msgResponceChannel :: Text
    }
     
instance FromJSON MessageResponse where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      MessageResponse <$> o .: "ts"
                      <*> o .: "channel"
    parseJSON x = fail $ show x
                         
data SlackTopic = SlackTopic
    { topicValue   :: Text
    , topicCreator :: Text
    , topicLastSet :: Int
    }

instance FromJSON SlackTopic where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      SlackTopic <$> o .: "value"
                 <*> o .: "creator"
                 <*> o .: "last_set"
    parseJSON x = fail $ show x

data SlackChannel = SlackChannel
    { chanId                 :: Text
    , chanName               :: Text
    , chanIsChannel          :: Bool
    , chanCreated            :: Int
    , chanCreator            :: Text
    , chanIsArchived         :: Bool
    , chanIsGeneral          :: Bool
    , chanMembers            :: [Text]
    , chanTopic              :: SlackTopic
    , chanPurpose            :: SlackTopic
    , chanIsMember           :: Bool
    , chanLastRead           :: Text
    , chanUnreadCount        :: Int
    , chanUnreadCountDisplay :: Int
    }

instance FromJSON SlackChannel where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      SlackChannel <$> o .: "id"
                   <*> o .: "name"
                   <*> o .: "is_channel"
                   <*> o .: "created"
                   <*> o .: "creator"
                   <*> o .: "is_archived"
                   <*> o .: "is_general"
                   <*> o .: "members"
                   <*> o .: "topic"
                   <*> o .: "purpose"
                   <*> o .: "is_member"
                   <*> o .: "last_read"
                   <*> o .: "unread_count"
                   <*> o .: "unread_count_display"
    parseJSON x = fail $ show x

data SlackFile = SlackFile
    { fileId :: Text
    , fileCreated :: Int
    , fileTimestamp :: Int
    , fileName :: Text
    , fileTitle :: Text
    , fileMimetype :: Text
    , fileFiletype :: Text
    , filePrettyType :: Text
    , fileUser :: Text
    , fileMode :: Text
    , fileEditable :: Bool
    , fileIsExternal :: Bool
    , fileExternalType :: Text
    , fileSize :: Int
    , fileUrl :: Text
    , fileUrlDownload :: Text
    , fileUrlPrivate :: Text
    , fileUrlPrivateDownload :: Text
    , fileThumb64 :: Text
    , fileThumb80 :: Text
    , fileThumb360 :: Text
    , fileThumb360Gif :: Text
    , fileThumb360W :: Text
    , fileThumb360H :: Text
    , filePermalink :: Text
    , fileEditLink :: Text
    , filePreview :: Text
    , filePreviewHighlight :: Text
    , fileLines :: Int
    , fileLinesMore :: Int
    , fileIsPublic :: Bool
    , filePublicUrlShared :: Text
    , fileChannels :: [Text]
    , fileGroups :: [Text]
    , fileNumStars :: Int
    , fileIsStarred :: Bool
    }
                       
instance FromJSON SlackFile where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      SlackFile <$> o .: "id"
                <*> o .: "created"
                <*> o .: "timestamp"
                <*> o .: "name"
                <*> o .: "title"
                <*> o .: "mimetype"
                <*> o .: "filetype"
                <*> o .: "pretty_type"
                <*> o .: "user"
                <*> o .: "mode"
                <*> o .: "editable"
                <*> o .: "is_external"
                <*> o .: "external_type"
                <*> o .: "size"
                <*> o .: "url"
                <*> o .: "url_download"
                <*> o .: "url_private"
                <*> o .: "url_private_download"
                <*> o .: "thumb_64"
                <*> o .: "thumb_80"
                <*> o .: "thumb_360"
                <*> o .: "thumb_360_gif"
                <*> o .: "thumb_360_w"
                <*> o .: "thumb_360_h"
                <*> o .: "permalink"
                <*> o .: "edit_link"
                <*> o .: "preview"
                <*> o .: "preview_highlight"
                <*> o .: "lines"
                <*> o .: "lines_more"
                <*> o .: "is_public"
                <*> o .: "public_url_shared"
                <*> o .: "channels"
                <*> o .: "groups"
                <*> o .: "num_stars"
                <*> o .: "is_starred"
    parseJSON x = fail $ show x

data SlackUser = SlackUser
    { userId :: Text
    , userName :: Text
    , userDeleted :: Bool
    , userColor :: Text
    , userProfile :: SlackProfile
    , userIsAdmin :: Bool
    , userIsOwner :: Bool
    , userIsPrimaryOwner :: Bool
    , userIsRestricted :: Bool
    , userIsUltraRestricted :: Bool
    , userHasFiles :: Bool
    }
                  
instance FromJSON SlackUser where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      SlackUser <$> o .: "id"
                    <*> o .: "name"
                    <*> o .: "deleted"
                    <*> o .: "color"
                    <*> o .: "profile"
                    <*> o .: "is_admin"
                    <*> o .: "is_owner"
                    <*> o .: "is_primary_owner"
                    <*> o .: "is_restricted"
                    <*> o .: "is_ultra_restricted"
                    <*> o .: "has_files"
    parseJSON x = fail $ show x

data SlackProfile = SlackProfile
    { userFirstName :: Text
    , userLastName :: Text
    , userRealName :: Text
    , userEmail :: Text
    , userSkype :: Text
    , userPhone :: Text
    , userImage24 :: Text
    , userImage32 :: Text
    , userImage48 :: Text
    , userImage72 :: Text
    , userImage192 :: Text
    }
instance FromJSON SlackProfile where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      SlackProfile <$> o .: "first_name"
                    <*> o .: "last_name"
                    <*> o .: "real_name"
                    <*> o .: "email"
                    <*> o .: "skype"
                    <*> o .: "phone"
                    <*> o .: "image_24"
                    <*> o .: "image_32"
                    <*> o .: "image_48"
                    <*> o .: "image_72"
                    <*> o .: "image_192"
    parseJSON x = fail $ show x

data Message = Message
    { messageType :: Text
    , messageChannel :: Text
    , messageUser :: Text
    , messageText :: Text
    , messageTs :: Text
    }
instance FromJSON Message where
    parseJSON (Object o) = do
      Message <$> o .: "type"
                  <*> o .:? "channel" .!= ""
                  <*> (o .:? "user") `or` (o .: "username")
                  <*> o .:  "text"
                  <*> o .:  "ts"
          where
            or m1 m2 = do
              m1' <- m1
              case m1' of
                Just x  -> return x
                Nothing -> m2
    parseJSON x = fail $ show x
                  
data StarItem = StarMessage
    { starMessageChannel :: Text
    , starMessageMessage :: Message
    }
              | StarFile
              | StarFileComment
              | StarChannel
              | StarIM
              | StarGroup
instance FromJSON StarItem where
    parseJSON (Object o) = do
      mtype <- o .: "type"
      case mtype :: Text of
        "message" -> StarMessage <$> o .: "channel"
                                 <*> o .: "message"
        _ -> fail "Slack Type: sorry unimplemented"
    parseJSON x = fail $ show x

data SlackEvent = EventHello
                | EventMessage Message
                | EventStarAdded
    { eventStarAddedUser :: Text
    , eventStarAddedItem :: StarItem
    , eventStarAddedEventTs :: Text
    }
                | EventStarRemoved
    { eventStarAddedUser :: Text
    , eventStarAddedItem :: StarItem
    , eventStarAddedEventTs :: Text
    }
                | EventOther Text

instance FromJSON SlackEvent where
    parseJSON (Object o) = do
      mtype <- o .: "type"
      case mtype :: Text of
        "hello" -> return EventHello
        "message" -> EventMessage <$> parseJSON (Object o)
        "star_added" -> EventStarAdded <$> o .: "user"
                                       <*> o .: "item"
                                       <*> o .: "event_ts"
        "star_removed" -> EventStarRemoved <$> o .: "user"
                                       <*> o .: "item"
                                       <*> o .: "event_ts"
        _ -> return $ EventOther $ pack $ show (Object o)
    parseJSON x = fail $ show x
