{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}

module DB where

import Import
import Database.Persist.Sql
import Data.AnimalParts

getRoundStartTime :: Handler UTCTime
getRoundStartTime = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundStartTime currRound

addPlayer :: PlayerId -> Handler Value
addPlayer p = do
    runDB $ update p [PlayerJoined =. True]
    return $ object ["status" .= ("success" :: Text)]

resetPlayers :: Handler ()
resetPlayers =
    runDB $ updateWhere [] [ PlayerJoined =. False
                           , PlayerSlot0 =. NoHead
                           , PlayerSlot1 =. NoBody
                           , PlayerSlot2 =. NoLeg]

startNewRound :: Handler ()
startNewRound = do
    currTime <- liftIO $ getCurrentTime
    let (roundId :: RoundId) = toSqlKey (fromIntegral (1 :: Int))
    _ <- resetPlayers
    runDB $ update roundId [RoundStartTime =. currTime] 
    return ()

getPlayers :: Handler (Player, Player)
getPlayers = do
    players :: [Entity Player ]<- runDB $ selectList [] []
    return $ case players of
      ((Entity _pid1 player1):(Entity _pid2 player2):[]) -> (player1, player2)
      _ -> error "Invalid DB state"
