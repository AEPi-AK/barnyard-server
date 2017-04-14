module Handler.Place where

import Import
import DB
import Data.AnimalParts

postPlaceR :: PlayerId -> Int -> String -> Handler Value 
postPlaceR playerId slot tileId = do
    status <- placeTile playerId slot (partForId tileId) 
    return $ object ["status" .= status]
