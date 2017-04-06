{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.AnimalParts

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON Player where
    toJSON (Player slot0 slot1 slot2 joined) = (object
      [ "slot0" .= show slot0
      , "slot1" .= show slot1
      , "slot2" .= show slot2
      , "joined" .= show joined
      ])
