{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
module Data.GameState where

import Import.NoFoundation
import Data.Time.Clock
import Data.AnimalParts

data GameState = GameState { currentPhase :: GamePhase
                           , timeSincePhaseStart :: NominalDiffTime
                           , player1 :: Player
                           , player2 :: Player
                           , location :: Location
                           , settings :: GameSettings
                           , winner :: Winner
                        } deriving (Show, Eq)

data PlayerJSON = PlayerJSON { player :: Player
                             , biome :: Location
                             , gPhase :: GamePhase }
instance ToJSON PlayerJSON where
    toJSON playerJSON = 
        let (Player slot0 slot1 slot2 joined) = player playerJSON in
        let isJoined = case gPhase playerJSON of
                            GameWaiting -> False
                            _ -> joined in
        (object
        [ "slot0" .= show slot0
        , "slot1" .= show slot1
        , "slot2" .= show slot2
        , "slot0Score" .= score (biome playerJSON) (Head slot0)
        , "slot1Score" .= score (biome playerJSON) (Body slot1)
        , "slot2Score" .= score (biome playerJSON) (Leg  slot2)
        , "joined" .= isJoined
        ])

data Winner = Tie | Player1 | Player2
    deriving(Show, Read, Eq)

instance ToJSON GameState where
    toJSON state = 
        let pTime = show $ phaseTime (currentPhase state) in
        let sinceStart = show $ timeSincePhaseStart state in
        let gamePhase = currentPhase state in
        (object 
            [ "currentPhase"        .= (show $ currentPhase state)
            , "phaseTime"           .= (Import.NoFoundation.take (Import.NoFoundation.length pTime - 1) pTime)
            , "timeSincePhaseStart" .= (Import.NoFoundation.take (Import.NoFoundation.length sinceStart - 1) sinceStart)
            , "player1"               .= (toJSON $ PlayerJSON { player = player1 state, biome = location state, gPhase = gamePhase})
            , "player2"               .= (toJSON $ PlayerJSON { player = player2 state, biome = location state, gPhase = gamePhase})
            , "location"               .= (show $ location state)
            , "winner"                 .= (show $ winner state)
            , "settings"               .= (toJSON $ settings state)
            ])

data GamePhase = GameJoining | GameWaiting | GameInProgress | GameScoring
               | GameInstructions | GameBiomeSelection
               | GameTimeUp | GameWinner | GameBiomePicking
    deriving(Show, Read, Eq)

data GameSettings = GameSettings { brightness :: Int
                                 , volume :: Int}
    deriving(Show, Read, Eq)
    
instance ToJSON GameSettings where
    toJSON settings = (object
        [ "brightness"    .=  (brightness settings)
        , "volume"        .=  (volume settings)
        ])

startPlusTime :: GamePhase -> NominalDiffTime
startPlusTime phase = (phaseStart phase) + (phaseTime phase)

phaseStart :: GamePhase -> NominalDiffTime
phaseStart GameJoining = 0
phaseStart GameInstructions = startPlusTime GameJoining
phaseStart GameBiomePicking = startPlusTime GameInstructions
phaseStart GameBiomeSelection = startPlusTime GameBiomePicking
phaseStart GameInProgress = startPlusTime GameBiomeSelection
phaseStart GameTimeUp = startPlusTime GameInProgress
phaseStart GameScoring = startPlusTime GameTimeUp
phaseStart GameWinner = startPlusTime GameScoring
phaseStart GameWaiting = startPlusTime GameWinner

phaseTime :: GamePhase -> NominalDiffTime
phaseTime GameJoining = 10
phaseTime GameInstructions = 17
phaseTime GameBiomePicking = 7
phaseTime GameBiomeSelection = 9
phaseTime GameInProgress = 31
phaseTime GameTimeUp = 5
phaseTime GameScoring = 10
phaseTime GameWinner = 5
phaseTime GameWaiting = 0

phaseForTimeDiff :: NominalDiffTime -> GamePhase
phaseForTimeDiff diff | diff > (phaseStart GameWaiting) = GameWaiting
                      | diff > (phaseStart GameWinner) = GameWinner
                      | diff > (phaseStart GameScoring) = GameScoring
                      | diff > (phaseStart GameTimeUp) = GameTimeUp
                      | diff > (phaseStart GameInProgress) = GameInProgress
                      | diff > (phaseStart GameBiomeSelection) = GameBiomeSelection
                      | diff > (phaseStart GameBiomePicking) = GameBiomePicking
                      | diff > (phaseStart GameInstructions) = GameInstructions
                      | diff >= (phaseStart GameJoining) = GameJoining
phaseForTimeDiff _ = error "Invalid time diff!"

timeInPhase :: NominalDiffTime -> GamePhase -> NominalDiffTime
timeInPhase time phase = time - (phaseStart phase)
                        
phaseAndTimeForStartTime :: UTCTime -> IO (GamePhase, NominalDiffTime)
phaseAndTimeForStartTime time = do
    currTime <- getCurrentTime
    let remSecs = diffUTCTime currTime time
    let phase = phaseForTimeDiff remSecs
    return (phase, timeInPhase remSecs phase)
