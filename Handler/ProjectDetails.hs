module Handler.ProjectDetails where

import Import
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Aeson.TH

import Handler.Voting

data VoteResponse = VoteResponse
    { remainingVotes :: Int
    , message :: Maybe Text
    }

$(deriveJSON defaultOptions ''VoteResponse)

postProjectDetailsR :: ProjectId -> Handler Value
postProjectDetailsR projectId = do
    project <- runDB $ get404 projectId
    remainingVotes <- getVotes $ projectHackday project
    if remainingVotes > 0
        then do
            setSession (remainingVotesKey $ projectHackday project) (T.pack $ show $ remainingVotes - 1)
            voteFor projectId
            returnJson $ VoteResponse {remainingVotes = remainingVotes - 1, message = Nothing }
        else do
            returnJson $ VoteResponse {remainingVotes = remainingVotes, message = Just "Out of votes!" }
            
    --let maybeVotes = lookupSession $ remainingVotesKey (projectHackday project)
    --in case maybeVotes of
    --    Just voteString -> do
    --                     let votes = read votes :: Int
    --    Nothing -> 
    --error "Not yet implemented: putProjectDetailsR"
