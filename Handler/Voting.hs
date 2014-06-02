module Handler.Voting where

import Import
import qualified Data.Text.Read as TR

readInt :: Text -> Maybe Int
readInt text = case TR.decimal text of
                Right (int,_) -> Just int
                Left _ -> Nothing -- Probably should return Nothing here, and default to 3

-- Gets the remaining votes for the given HackDay
-- If not present in the session, sets it to the default value.
getVotes :: HackDayId -> Handler Int
getVotes hackDayId = do
    maybeVotesText <- lookupSession $ remainingVotesKey hackDayId
    case maybeVotesText >>= readInt of
        Just votes -> return votes
        Nothing -> do
            setSession (remainingVotesKey hackDayId) defaultVotesText
            return defaultVotes

voteFor :: ProjectId -> Handler ()
voteFor projectId = runDB $ updateWhere [ProjectId ==. projectId] [ProjectVotes +=. 1]