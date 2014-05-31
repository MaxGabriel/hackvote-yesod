module Handler.ProjectDetails where

import Import
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import App.Voting

postProjectDetailsR :: ProjectId -> Handler Html
postProjectDetailsR projectId = do
    project <- runDB $ get404 projectId
    remainingVotes <- getVotes $ projectHackday project
    if remainingVotes > 0
        then do
            setSession (remainingVotesKey $ projectHackday project) (T.pack $ show $ remainingVotes - 1)
            voteFor projectId
            setMessage "Vote successful!"
            redirect $ HackDayDetailsR $ projectHackday project
        else do
            setMessage "Out of votes!"
            redirect $ HackDayDetailsR $ projectHackday project
    --let maybeVotes = lookupSession $ remainingVotesKey (projectHackday project)
    --in case maybeVotes of
    --    Just voteString -> do
    --                     let votes = read votes :: Int
    --    Nothing -> 
    --error "Not yet implemented: putProjectDetailsR"
