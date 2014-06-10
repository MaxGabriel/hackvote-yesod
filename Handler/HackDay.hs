module Handler.HackDay where

import Import
import Data.Time.Clock

data HackDayForm = HackDayForm
    { formTitle :: Text
    }
  deriving Show


titleSettings = FieldSettings
    { fsLabel = "Title"
    , fsTooltip = Nothing
    , fsId = Just "titleField"
    , fsName = Nothing
    , fsAttrs = [("placeholder","June Hackday"),("style","text-align: center;")]
    }

hackDayForm :: Maybe HackDayForm -> AForm Handler HackDayForm
hackDayForm mForm = HackDayForm
        <$> areq textField titleSettings (formTitle <$> mForm)

getHackDayR :: Handler Html
getHackDayR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap (hackDayForm Nothing)
    allHackdays <- runDB $ selectList [] [Desc HackDayCreated]
    defaultLayout $ do
        setTitle "Hackday!"
        $(widgetFile "listhackdays")

postHackDayR :: Handler Html
postHackDayR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap (hackDayForm Nothing)
    case res of
        FormSuccess hackForm -> do
                            currentTime <- liftIO $ getCurrentTime
                            hackId <- runDB $ insert $ HackDay { hackDayTitle = formTitle hackForm
                                                               , hackDayCreated = currentTime
                                                               , hackDayVotingClosed = False }
                            redirect (HackDayDetailsR hackId)

        _                    -> do
                            allHackdays <- runDB $ selectList [] [Desc HackDayCreated]
                            defaultLayout $(widgetFile "listhackdays")


