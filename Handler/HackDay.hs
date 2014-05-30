module Handler.HackDay where

import Import
import Data.Time.Clock
import Model

data HackDayForm = HackDayForm
    { formTitle :: Text
    }
  deriving Show

hackDayForm :: Maybe HackDayForm -> AForm Handler HackDayForm
hackDayForm mForm = HackDayForm
        <$> areq textField "Title" (formTitle <$> mForm)

getHackDayR :: Handler Html
getHackDayR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap (hackDayForm Nothing)
    allHackdays <- runDB $ selectList ([] :: [Filter HackDay]) [Desc HackDayCreated]
    liftIO $ print allHackdays
    defaultLayout $ do
        setTitle "Hackday!"
        $(widgetFile "listhackdays")

postHackDayR :: Handler Html
postHackDayR = undefined
    --do
    --((res, widget), enctype) <- runFormPost $ renderBootstrap (hackDayForm Nothing)
    --case res of
    --    FormSuccess hackForm -> do 
    --                        currentTime <- liftIO $ getCurrentTime
    --                        _ <- runDB $ insert $ HackDay (title hackForm) currentTime
    --                        defaultLayout [whamlet|"Success!"|]

    --    _                    -> defaultLayout $(widgetFile "listhackdays")


--postUserR :: Handler Html
--postUserR = do
--    ((res, widget), enctype) <- runFormPost $ renderBootstrap (userForm Nothing)
--    --runDB $ insert $ User "Max" Nothing
--    case res of 
--        FormSuccess user -> do
--                        hashed <- passwordHash (HashDB.defaultStrength) (password user)
--                        _ <- runDB $ insert $ User (username user) (hashed) (email user)
--                        defaultLayout [whamlet|"Success!"|]
--        _                -> defaultLayout $(widgetFile "signup")