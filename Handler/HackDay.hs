module Handler.HackDay where

import Import
import Data.Time.Clock
import Handler.Voting (setOwner)

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

currentHackday :: [Entity HackDay] -> Maybe (Entity HackDay)
currentHackday hackdays = headMaybe hackdays >>= openHackday

openHackday :: Entity HackDay -> Maybe (Entity HackDay)
openHackday hackday@(Entity id day) = if hackDayVotingOpen day
                                        then Just hackday
                                        else Nothing

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

tailEmpty :: [a] -> [a]
tailEmpty [] = []
tailEmpty (_:xs) = xs

currentAndPastHackdays :: [Entity HackDay] -> (Maybe (Entity HackDay), [Entity HackDay])
currentAndPastHackdays xs = 
    case currentHackday xs of
        Nothing -> (Nothing, xs)
        Just x -> (Just x, tailEmpty xs)


getHackDayR :: Handler Html
getHackDayR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap (hackDayForm Nothing)
    allHackdays <- runDB $ selectList [] [Desc HackDayCreated]
    let (currentHackday, pastHackdays) = currentAndPastHackdays allHackdays
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
                            setOwner hackId
                            redirect (HackDayDetailsR hackId)

        _                    -> do
                            allHackdays <- runDB $ selectList [] [Desc HackDayCreated]
                            let (currentHackday, pastHackdays) = currentAndPastHackdays allHackdays
                            defaultLayout $(widgetFile "listhackdays")


