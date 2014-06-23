module Model where

import Prelude
import Yesod
import Data.Text (Text,pack)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

defaultVotes :: Int
defaultVotes = 3

defaultVotesText :: Text
defaultVotesText = pack $ show defaultVotes

remainingVotesKey :: HackDayId -> Text
remainingVotesKey hackDayId = pack $ "remainingVotes" ++ show hackDayId

hackDayVotingOpen :: HackDay -> Bool
hackDayVotingOpen hackday = not $ hackDayVotingClosed hackday