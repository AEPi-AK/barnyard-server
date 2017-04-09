{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}

module DB where

import Import
import Database.Persist.Sql
import Data.AnimalParts
import System.Random
import qualified Data.List as L

getRoundStartTime :: Handler UTCTime
getRoundStartTime = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundStartTime currRound

getRoundLocation :: Handler Location 
getRoundLocation = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundLocation currRound

getVolume :: Handler Int
getVolume = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundVolume currRound

getBrightness :: Handler Int
getBrightness = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundBrightness currRound

getRound :: Handler Round
getRound = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ currRound

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
    randTime <- liftIO $ (randomIO :: IO Int)
    let idx = randTime `mod` (length locations)
    let (roundId :: RoundId) = toSqlKey (fromIntegral (1 :: Int))
    _ <- resetPlayers
    runDB $ update roundId [ RoundStartTime =. currTime
                           , RoundLocation =. (locations L.!! idx)
                           ] 
    return ()

updateVolume :: Int -> Handler ()
updateVolume volume = do
    runDB $ updateWhere [] [RoundVolume =. volume]

updateBrightness :: Int -> Handler ()
updateBrightness volume = do
    runDB $ updateWhere [] [RoundBrightness =. volume]

getPlayers :: Handler (Player, Player)
getPlayers = do
    players :: [Entity Player ]<- runDB $ selectList [] []
    return $ case players of
      ((Entity _pid1 player1):(Entity _pid2 player2):[]) -> (player1, player2)
      _ -> error "Invalid DB state"

placeTile :: PlayerId -> Int -> AnimalPart -> Handler ()
placeTile pid slot part = do
    case (slot, part) of
        (0, Head h) -> runDB $ update pid [PlayerSlot0 =. h]
        (1, Body b) -> runDB $ update pid [PlayerSlot1 =. b]
        (2, Leg  l) -> runDB $ update pid [PlayerSlot2 =. l]
        (0, _) -> runDB $ update pid [PlayerSlot0 =. HeadErr]
        (1, _) -> runDB $ update pid [PlayerSlot1 =. BodyErr]
        (2, _) -> runDB $ update pid [PlayerSlot2 =. LegErr]
        _ -> error "Invalid slot"

removeTile :: PlayerId -> Int -> Handler ()
removeTile pid slot = do
    case slot of
        0 -> runDB $ update pid [PlayerSlot0 =. NoHead]
        1 -> runDB $ update pid [PlayerSlot1 =. NoBody]
        2 -> runDB $ update pid [PlayerSlot2 =. NoLeg]
        _ -> error "Invalid slot"
