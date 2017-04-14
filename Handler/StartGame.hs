module Handler.StartGame where

import Import
import DB
import Database.Persist.Sql
import Data.GameState

postStartGameR :: Handler Value
postStartGameR = do
    _ <- startNewRoundTesting (phaseStart GameInProgress)
    addPlayer (toSqlKey (fromIntegral (1 :: Int)))
    addPlayer (toSqlKey (fromIntegral (2 :: Int)))
    return $ object ["status" .= ("success" :: Text)]

    
