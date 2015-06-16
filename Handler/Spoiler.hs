module Handler.Spoiler where

import Import
import qualified Data.Text as T
-- token=gNXpxF6JBAebdhGEPpGX1bs0
-- team_id=T0001
-- team_domain=example
-- channel_id=C2147483705
-- channel_name=test
-- user_id=U2147483697
-- user_name=Steve
-- command=/weather
-- text=94070

data SpoilerRequest = SpoilerRequest
    { token :: Text
    , teamId :: Text
    , teamDomain :: Text
    , channelId :: Text
    , channelName :: Text
    , userId :: Text
    , userName :: Text
    , text :: Text
    }
  deriving Show

data SlackMessage = SlackMessage
    { 
    }

requirePostParam :: Text -> Handler Text
requirePostParam key = do
    mParam <- lookupPostParam key
    case mParam of
        Just value -> return value
        Nothing -> invalidArgs ["Missing POST param: " ++ key]

 
formatSpoiler :: Text -> Text
formatSpoiler = T.strip . T.dropWhile (== ':') 

postSpoilerR :: Handler Text
postSpoilerR = do
    $(logDebug) "Got POST to /spoiler"
    body <- runRequestBody
    $(logDebug) ("Request body:" ++ tshow (fst body))

    mCommand <- lookupPostParam "command"

    $(logDebug) ("Maybe Command:" ++ tshow mCommand)

    token <- requirePostParam "token"
    teamId <- requirePostParam "team_id"
    teamDomain <- requirePostParam "team_domain"
    channelId <- requirePostParam "channel_id"
    channelName <- requirePostParam "channel_name"
    userId <- requirePostParam "user_id"
    userName <- requirePostParam "user_name"
    text <- requirePostParam "text"

    let spoilerRequest = SpoilerRequest {..}
    $(logDebug) ("SpoilerRequest is:" ++ tshow spoilerRequest)    

    let response = case T.breakOn ":" text of
                        (_, "") -> "Please format your message as \"Spoiler description: Actual spoiler\""
                        (description, spoiler) -> formatSpoiler spoiler

    return response
    
    -- ((res, widget), enctype) <- runFormPost $ renderBootstrap (projectForm Nothing)<- runFormPostNoToken

    -- return ""

-- lookupParam "key" -- specify if it needs to be present or not?