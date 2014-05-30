module Handler.HackDay where

import Import

data HackDayForm = HackDayForm
    { title :: Text 
    }
  deriving Show

hackDayForm :: Maybe HackDayForm -> AForm Handler HackDayForm
hackDayForm mForm = HackDayForm
        <$> areq textField "Title" (title <$> mForm)

getHackDayR :: Handler Html
getHackDayR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap (hackDayForm Nothing)
    defaultLayout $ do
        setTitle "Hackday!"
        $(widgetFile "listhackdays")

postHackDayR :: Handler Html
postHackDayR = error "Not yet implemented: postHackDayR"
