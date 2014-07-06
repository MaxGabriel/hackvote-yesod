{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import TestImport
import qualified Data.List as L
import Data.Time.Clock
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (rawExecute)

homeSpecs :: Spec
homeSpecs =
    ydescribe "These are some example tests" $ do

        yit "loads the index and checks it looks right" $ do
            _ <- runDB $ rawExecute "TRUNCATE TABLE hack_day, project;" []
            get HackDayR
            statusIs 200
            htmlAnyContain "h2" "New Hackday"

        yit "shows the current hackday" $ do
            _ <- runDB $ rawExecute "TRUNCATE TABLE hack_day, project;" []
            currentTime <- liftIO $ getCurrentTime
            hackId <- runDB $ insert $ HackDay { hackDayTitle = "testTitle"
                                               , hackDayCreated = currentTime
                                               , hackDayVotingClosed = False }
            get HackDayR
            htmlAllContain ".currentHackday" "testTitle"

        yit "doesn't show the current hackday if closed" $ do
            _ <- runDB $ rawExecute "TRUNCATE TABLE hack_day, project;" []
            currentTime <- liftIO $ getCurrentTime
            hackId <- runDB $ insert $ HackDay { hackDayTitle = "testTitle"
                                               , hackDayCreated = currentTime
                                               , hackDayVotingClosed = True }
            get HackDayR
            htmlNoneContain ".currentHackday" "testTitle"

        yit "shows the past hackdays" $ do
            _ <- runDB $ rawExecute "TRUNCATE TABLE hack_day, project;" []
            currentTime <- liftIO $ getCurrentTime
            hackId <- runDB $ insert $ HackDay { hackDayTitle = "test1"
                                               , hackDayCreated = currentTime
                                               , hackDayVotingClosed = True }

            hackId <- runDB $ insert $ HackDay { hackDayTitle = "test2"
                                               , hackDayCreated = currentTime
                                               , hackDayVotingClosed = True }

            get HackDayR
            htmlAnyContain "a" "test1"
            htmlAnyContain "a" "test1"


