module Handler.Remove where

import Import
import DB

postRemoveR :: PlayerId -> Int -> Handler Value 
postRemoveR playerId slot = do
    _ <- removeTile playerId slot
    return $ object ["status" .= ("success" :: Text)]
