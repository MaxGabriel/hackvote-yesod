module Handler.Project where

import Import
import Handler.HackDayDetails
import Data.Time.Clock
import Handler.Voting (getVotes)

postProjectR :: HackDayId -> Handler Html
postProjectR hackDayID = do
    hackDay <- runDB $ get404 hackDayID
    ((res, widget), enctype) <- runFormPost $ renderBootstrap (projectForm Nothing)
    case res of
        FormSuccess form -> do
                        currentTime <- liftIO $ getCurrentTime
                        _ <- runDB $ insert $ Project hackDayID (name form) (creators form) 0 currentTime
                        redirect $ HackDayDetailsR hackDayID
        _                -> do
                        remainingVotes <- getVotes hackDayID
                        projects <- runDB $ selectList ([ProjectHackday ==. hackDayID]) [Asc ProjectId]
                        defaultLayout $ $(widgetFile "hackday")
    
