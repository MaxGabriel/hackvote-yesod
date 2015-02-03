module Handler.HackDayDetails where

import Import

import qualified Handler.Voting as Voting

data ProjectForm = ProjectForm
    { name :: Text
    , creators :: Text
    }
  deriving Show

projectForm :: Maybe ProjectForm -> AForm Handler ProjectForm
projectForm mForm = ProjectForm
        <$> areq textField "Name" (name <$> mForm)
        <*> areq textField "Creators" (creators <$> mForm)

getHackDayDetailsR :: HackDayId -> Handler Html
getHackDayDetailsR hackDayID = do
    remainingVotes <- Voting.getVotes hackDayID
    hackDay <- runDB $ get404 hackDayID
    isOwner <- Voting.isOwner hackDayID
    let votingClosed = hackDayVotingClosed hackDay
        sortCriteria = if votingClosed then [Desc ProjectVotes] else [Asc ProjectId]
    projects <- runDB $ selectList [ProjectHackday ==. hackDayID] sortCriteria
    (widget, enctype) <- generateFormPost $ renderBootstrap (projectForm Nothing)
    defaultLayout $ do
        setTitle $ toHtml $ hackDayTitle hackDay
        $(widgetFile "hackday")

postCloseHackDayR :: HackDayId -> Handler Html
postCloseHackDayR hackDayID = do
    isOwner <- Voting.isOwner hackDayID
    if isOwner
        then do
            runDB $ update hackDayID [HackDayVotingClosed =. True]
            redirect (HackDayDetailsR hackDayID)
        else redirect (HackDayDetailsR hackDayID)