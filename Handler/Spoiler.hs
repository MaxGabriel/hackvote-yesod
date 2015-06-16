module Handler.Spoiler where

import Import
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HC
import Data.Aeson.TH
import qualified Data.Aeson as A
import Network.HTTP.Types.Status

data Spoiler = Spoiler
    { spoilerDescription :: Text
    , spoilerText :: Text
    }

requirePostParam :: Text -> Handler Text
requirePostParam key = do
    mParam <- lookupPostParam key
    case mParam of
        Just value -> return value
        Nothing -> invalidArgs ["Missing POST param: " ++ key]

 
formatSpoiler :: Text -> Text
formatSpoiler = T.strip . T.dropWhile (== ':') 

postSpoilerToSlack :: Spoiler -> Text -> Handler Status
postSpoilerToSlack spoiler channel = do
    let slackMessage = A.object [ "text" A..= ("Spoiler for: " ++ spoilerDescription spoiler)
                                , "channel" A..= channel
                                , ("unfurl_links","false")
                                , ("unfurl_media","false")
                                , "attachments" A..= [ A.object [("text" :: Text) A..= ("\n\n\n\n\n" ++ (spoilerText spoiler)) ] ]
                                ]
    slackUrl <- appSlackUrl <$> appSettings <$> getYesod

    initReq <- HC.parseUrl (T.unpack slackUrl)
    let req2 = initReq { HC.requestBody = RequestBodyLBS $ A.encode slackMessage, HC.checkStatus = \_ _ _ -> Nothing }
    app <- getYesod

    response <- HC.httpLbs req2 (appHttpManager app)
    $(logDebug) ("Slack Response = " ++ tshow response)
    return $ responseStatus response

checkSlackToken :: Text -> Handler ()
checkSlackToken token = do
    settingsToken <- appSlackToken <$> appSettings <$> getYesod
    if settingsToken == token
        then return ()
        else permissionDenied ("Invalid Slack token: " ++ token)


postSpoilerR :: Handler Text
postSpoilerR = do
    $(logDebug) "Got POST to /spoiler"

    token <- requirePostParam "token"
    checkSlackToken token

    spoilerTeamId <- requirePostParam "team_id"
    spoilerTeamDomain <- requirePostParam "team_domain"
    spoilerChannelId <- requirePostParam "channel_id"
    spoilerChannelName <- requirePostParam "channel_name"
    spoilerUserId <- requirePostParam "user_id"
    spoilerUserName <- requirePostParam "user_name"
    rawText <- requirePostParam "text"

    let (description, formattedSpoilerText) = case T.breakOn ":" rawText of
                        (spoiler, "") -> ("", spoiler)
                        (description, spoiler) -> (description, formatSpoiler spoiler)


    let spoiler = Spoiler description formattedSpoilerText

    status <- postSpoilerToSlack spoiler spoilerChannelName

    case statusIsSuccessful status of
        True -> return ""
        False -> return "Error posting to Slack. Bug Max about this?"