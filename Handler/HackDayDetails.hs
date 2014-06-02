module Handler.HackDayDetails where

import Import

import Handler.Voting (getVotes)

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
    remainingVotes <- getVotes hackDayID
    hackDay <- runDB $ get404 hackDayID
    projects <- runDB $ selectList ([ProjectHackday ==. hackDayID]) [Asc ProjectId]
    (widget, enctype) <- generateFormPost $ renderBootstrap (projectForm Nothing)
    defaultLayout $ do
        setTitle $ toHtml $ hackDayTitle hackDay
        $(widgetFile "hackday")
