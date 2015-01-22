module Handler.HomeSpec (spec) where

import TestImport
import Data.Time.Clock
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (rawExecute)

spec :: Spec
spec = withApp $ do
    describe "These are some example tests" $ do

        it "loads the index and checks it looks right" $ do
            get HackDayR
            statusIs 200
            htmlAnyContain "h2" "New Hackday"

        it "shows the current hackday" $ do
            currentTime <- liftIO $ getCurrentTime
            _ <- runDB $ insert $ HackDay { hackDayTitle = "testTitle"
                                          , hackDayCreated = currentTime
                                          , hackDayVotingClosed = False }
            get HackDayR
            htmlAllContain ".currentHackday" "testTitle"

        it "doesn't show the current hackday if closed" $ do
            currentTime <- liftIO $ getCurrentTime
            _ <- runDB $ insert $ HackDay { hackDayTitle = "testTitle"
                                          , hackDayCreated = currentTime
                                          , hackDayVotingClosed = True }
            get HackDayR
            htmlNoneContain ".currentHackday" "testTitle"

        it "shows the past hackdays" $ do
            currentTime <- liftIO $ getCurrentTime
            _ <- runDB $ insert $ HackDay { hackDayTitle = "test1"
                                          , hackDayCreated = currentTime
                                          , hackDayVotingClosed = True }

            _ <- runDB $ insert $ HackDay { hackDayTitle = "test2"
                                          , hackDayCreated = currentTime
                                          , hackDayVotingClosed = True }

            get HackDayR
            htmlAnyContain "a" "test1"
            htmlAnyContain "a" "test2"