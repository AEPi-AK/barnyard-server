module Handler.Place where

import Import
import DB
import Data.AnimalParts

postPlaceR :: PlayerId -> Int -> String -> Handler Value 
postPlaceR playerId slot tileId = do
    let part = partForId tileId
    _ <- placeTile playerId slot part
    let status = if partError part then ("error" :: Text) else ("success" :: Text)
    return $ object ["status" .= status]
