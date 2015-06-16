module Handler.Spoiler where

import Import

-- token=gNXpxF6JBAebdhGEPpGX1bs0
-- team_id=T0001
-- team_domain=example
-- channel_id=C2147483705
-- channel_name=test
-- user_id=U2147483697
-- user_name=Steve
-- command=/weather
-- text=94070

-- data SpoilerRequest = SpoilerRequest
--     { token :: Text
--     , team_id :: 
--     }
--   deriving Show

postSpoilerR :: Handler ()
postSpoilerR = do
    $(logDebug) "Got POST to /spoiler"
    body <- runRequestBody
    $(logDebug) ("Request body:" ++ tshow (fst body))

    mCommand <- lookupPostParam "command"

    $(logDebug) ("Maybe Command:" ++ tshow mCommand)

    -- ((res, widget), enctype) <- runFormPost $ renderBootstrap (projectForm Nothing)<- runFormPostNoToken

    return ()
