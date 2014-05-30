module Handler.Project where

import Import
import Handler.HackDayDetails

postProjectR :: HackDayId -> Handler Html
postProjectR hackDayID = do
    hackday <- runDB $ get404 hackDayID
    ((res, widget), enctype) <- runFormPost $ renderBootstrap (projectForm Nothing)
    case res of
        FormSuccess form -> do
                        runDB $ insert $ Project hackDayID (name form) (creators form)
                        redirect $ HackDayDetailsR hackDayID
        _                -> do
                        projects <- runDB $ selectList ([ProjectHackday ==. hackDayID]) []
                        defaultLayout $ $(widgetFile "hackday")
    
