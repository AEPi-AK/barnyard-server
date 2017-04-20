{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}

module DB where

{-@ LIQUID "--prune-unsorted" @-}

import Import
import Database.Persist.Sql
import Data.AnimalPartsSpec
import Data.GameState
import Data.Time
import System.Random
import qualified Data.List as L

{-@ data Round = Round { roundStartTime :: UTCTime
                       , roundLocation :: Location
                       , roundVolume :: {v:Int | v >= 0 && v <= 100}
                       , roundBrightness :: {b:Int | b >=0 && b <= 255}
                       } 
@-}


getRoundStartTime :: Handler UTCTime
getRoundStartTime = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundStartTime currRound

getRoundLocation :: Handler Location 
getRoundLocation = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundLocation currRound

{-@ getVolume :: Handler {v:Int | v <= 100 && v >= 0} @-}
getVolume :: Handler Int
getVolume = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundVolume currRound

{-@ getBrightness :: Handler {v:Int | v >=0 && v <= 255} @-}
getBrightness :: Handler Int
getBrightness = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundBrightness currRound

getRound :: Handler Round
getRound = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ currRound

addPlayer :: PlayerId -> Handler () 
addPlayer p = do
    runDB $ update p [PlayerJoined =. True]
    return () 

resetPlayers :: Handler ()
resetPlayers =
    runDB $ updateWhere [] [ PlayerJoined =. False
                           , PlayerSlot0 =. NoHead
                           , PlayerSlot1 =. NoBody
                           , PlayerSlot2 =. NoLeg
                           , PlayerSlot0Score =. 0
                           , PlayerSlot1Score =. 0
                           , PlayerSlot2Score =. 0
                           ]

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

startNewRoundTesting :: NominalDiffTime -> Handler ()
startNewRoundTesting offset = do
    currTime <- liftIO $ getCurrentTime
    let currTime' = addUTCTime (-offset) currTime 
    randTime <- liftIO $ (randomIO :: IO Int)
    let idx = randTime `mod` (length locations)
    let (roundId :: RoundId) = toSqlKey (fromIntegral (1 :: Int))
    _ <- resetPlayers
    runDB $ update roundId [ RoundStartTime =. currTime'
                           , RoundLocation =. (locations L.!! idx)
                           ] 
    return ()

resetGame :: Handler ()
resetGame = do
    currTime <- liftIO $ getCurrentTime
    _ <- resetPlayers
    let (roundId :: RoundId) = toSqlKey (fromIntegral (1 :: Int))
    runDB $ update roundId [ RoundStartTime =. (addUTCTime ((realToFrac $ -(1000 :: Integer))) currTime)
                           ] 
    return ()

{-@ updateVolume :: {v:Int | v >= 0 && v <= 100} -> Handler () @-}
updateVolume :: Int -> Handler ()
updateVolume volume = do
    runDB $ updateWhere [] [RoundVolume =. volume]

{-@ updateBrightness :: {v:Int | v >= 0 && v <= 255} -> Handler () @-}
updateBrightness :: Int -> Handler ()
updateBrightness volume = do
    runDB $ updateWhere [] [RoundBrightness =. volume]

getPlayers :: Handler (Player, Player)
getPlayers = do
    players :: [Entity Player ]<- runDB $ selectList [] []
    return $ case players of
      ((Entity _pid1 player1):(Entity _pid2 player2):[]) -> (player1, player2)
      xs -> error ("Invalid DB state: " Import.++ show xs)

placeTile :: PlayerId -> Int -> AnimalPart -> Handler Text
placeTile pid slot part = do
    loc <- getRoundLocation
    case (slot, part) of
        (0, Head h) -> do
            runDB $ update pid [PlayerSlot0 =. h, PlayerSlot0Score =. (score loc part)]
            return "success"
        (1, Body b) -> do
            runDB $ update pid [PlayerSlot1 =. b, PlayerSlot1Score =. (score loc part)]
            return "success"
        (2, Leg  l) -> do
            runDB $ update pid [PlayerSlot2 =. l, PlayerSlot2Score =. (score loc part)]
            return "success"
        (0, _) -> do
            runDB $ update pid [PlayerSlot0 =. HeadErr, PlayerSlot0Score =. (score loc part)]
            return "error"
        (1, _) -> do
            runDB $ update pid [PlayerSlot1 =. BodyErr, PlayerSlot1Score =. (score loc part)]
            return "error"
        (2, _) -> do
            runDB $ update pid [PlayerSlot2 =. LegErr, PlayerSlot2Score =. (score loc part)]
            return "error"
        _ -> error "Invalid slot"

removeTile :: PlayerId -> Int -> Handler ()
removeTile pid slot = do
    currRound <- getRound
    (phase, _elapsed) <- liftIO $ phaseAndTimeForStartTime (roundStartTime currRound)
    let extra = if (phase == GameTimeUp || phase == GameWinner || phase == GameScoring) then [] else [0]
    case slot of
        0 -> runDB $ update pid ([PlayerSlot0 =. NoHead] ++ (map (\x -> PlayerSlot0Score =. x) extra))
        1 -> runDB $ update pid ([PlayerSlot1 =. NoBody] ++ (map (\x -> PlayerSlot1Score =. x) extra))
        2 -> runDB $ update pid ([PlayerSlot2 =. NoLeg] ++ (map (\x -> PlayerSlot2Score =. x) extra))
        _ -> error "Invalid slot"
