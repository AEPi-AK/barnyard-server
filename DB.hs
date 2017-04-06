{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module DB where

import Import
import Database.Persist.Sql

getRoundStartTime :: Handler UTCTime
getRoundStartTime = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundStartTime currRound

startNewRound :: Handler ()
startNewRound = do
    currTime <- liftIO $ getCurrentTime
    let (roundId :: RoundId) = toSqlKey (fromIntegral (1 :: Int))
    runDB $ update roundId [RoundStartTime =. currTime] 
    return ()

getPlayers :: Handler (Player, Player)
getPlayers = do
    players :: [Entity Player ]<- runDB $ selectList [] []
    return $ case players of
      ((Entity _pid1 player1):(Entity _pid2 player2):[]) -> (player1, player2)
      _ -> error "Invalid DB state"
