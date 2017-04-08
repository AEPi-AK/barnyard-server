module Handler.Reset where

import Import
import DB

postResetR :: Handler Value
postResetR = do
    _ <- startNewRound 
    return $ object ["status" .= ("success" :: Text)]
