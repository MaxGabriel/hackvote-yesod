module Handler.Project where

import Import
import Handler.HackDayDetails
import Data.Time.Clock

postProjectR :: HackDayId -> Handler Html
postProjectR hackDayID = do
    hackday <- runDB $ get404 hackDayID
    ((res, widget), enctype) <- runFormPost $ renderBootstrap (projectForm Nothing)
    case res of
        FormSuccess form -> do
                        currentTime <- liftIO $ getCurrentTime
                        runDB $ insert $ Project hackDayID (name form) (creators form) 0 currentTime
                        redirect $ HackDayDetailsR hackDayID
        _                -> do
                        projects <- runDB $ selectList ([ProjectHackday ==. hackDayID]) [Asc ProjectId]
                        defaultLayout $ $(widgetFile "hackday")
    
