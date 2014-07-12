{-# LANGUAGE MultiWayIf #-}

module Handler.ProjectDetails where

import Import
import qualified Data.Text as T
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
    hackday <- runDB $ get404 $ projectHackday project
    remainingVotes <- getVotes $ projectHackday project
    if | remainingVotes == 0 -> returnJson $ VoteResponse {remainingVotes = remainingVotes, message = Just "Out of votes!" }
       | hackDayVotingClosed hackday -> returnJson $ VoteResponse {remainingVotes = remainingVotes, message = Just "Voting closed" }
       | otherwise -> do
            setSession (remainingVotesKey $ projectHackday project) (T.pack $ show $ remainingVotes - 1)
            voteFor projectId
            returnJson $ VoteResponse {remainingVotes = remainingVotes - 1, message = Nothing }
                
