module Handler.StartGame where

import Import
import DB
import Database.Persist.Sql

postStartGameR :: Handler Value
postStartGameR = do
    _ <- startNewRound
    addPlayer (toSqlKey (fromIntegral (1 :: Int)))
    addPlayer (toSqlKey (fromIntegral (2 :: Int)))
    return $ object ["status" .= ("success" :: Text)]

    
