module Handler.HomeSpec (spec) where

import TestImport
import Data.Time.Clock
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (rawExecute)
import Safe

spec :: Spec
spec = withApp $ do
    describe "These are some example tests" $ do

        it "loads the index and checks it looks right" $ do
            get HackDayR
            statusIs 200
            htmlAnyContain "h2" "New Hackday"

        it "shows the current hackday" $ do
            currentTime <- liftIO getCurrentTime
            _ <- runDB $ insert HackDay { hackDayTitle = "testTitle"
                                          , hackDayCreated = currentTime
                                          , hackDayVotingClosed = False }
            get HackDayR
            htmlAllContain ".currentHackday" "testTitle"

        it "doesn't show the current hackday if closed" $ do
            currentTime <- liftIO getCurrentTime
            _ <- runDB $ insert HackDay { hackDayTitle = "testTitle"
                                          , hackDayCreated = currentTime
                                          , hackDayVotingClosed = True }
            get HackDayR
            htmlNoneContain ".currentHackday" "testTitle"

        it "shows the past hackdays" $ do
            currentTime <- liftIO getCurrentTime
            _ <- runDB $ insert HackDay { hackDayTitle = "test1"
                                          , hackDayCreated = currentTime
                                          , hackDayVotingClosed = True }

            _ <- runDB $ insert HackDay { hackDayTitle = "test2"
                                          , hackDayCreated = currentTime
                                          , hackDayVotingClosed = True }

            get HackDayR
            htmlAnyContain "a" "test1"
            htmlAnyContain "a" "test2"
    describe "Hackday creation" $ do
        it "creates a hackday" $ do
            get HackDayR
            statusIs 200

            request $ do
                setUrl HackDayR
                setMethod "POST"
                addNonce
                byLabel "Title" "June Hackday"
            statusIs 303
            
            allHackdays <- runDB $ selectList [] [Desc HackDayCreated]
            assertEqual "should be 1 hackday" 1 (length allHackdays)
            assertEqual "should be named June Hackday" (hackDayTitle $ entityVal $ headNote "empty list" allHackdays) "June Hackday"

        it "doesn't create hackdays without names" $ do
            get HackDayR
            statusIs 200
            request $ do
                setUrl HackDayR
                setMethod "POST"
                addNonce
            statusIs 200
            allHackdays <- runDB $ selectList [] [Desc HackDayCreated]
            assertEqual "should be no hackdays" 0 (length allHackdays)