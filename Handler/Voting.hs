module Handler.Voting where

import Import
import qualified Data.Text.Read as TR
import Data.Maybe (isJust)
-- import Data.Text (pack)
import Control.Monad (liftM)

-- Hackday Ownership goes to the creator, so that they can close voting.

setOwner :: HackDayId -> Handler ()
setOwner hackDayId = setSession (ownerKey hackDayId) "" -- unused value; I just use presence of the value to denote ownership

isOwner :: HackDayId -> Handler Bool
isOwner hackDayId = liftM isJust (lookupSession $ ownerKey hackDayId)

ownerKey :: HackDayId -> Text
ownerKey hackDayId = pack $ "hackDayOwner" ++ show hackDayId

-- Individuals can vote on 3 projects per hackday; remaining votes are stored in a session

readInt :: Text -> Maybe Int
readInt text = case TR.decimal text of
                Right (int,_) -> Just int
                Left _ -> Nothing 

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
voteFor projectId = runDB $ update projectId [ProjectVotes +=. 1]