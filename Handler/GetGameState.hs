module Handler.GetGameState where

import Import
import DB
import Data.GameState
import Data.AnimalParts

getGetGameStateR :: Handler Value
getGetGameStateR = do 
    (player1, player2) <- getPlayers
    currRound <- getRound
    let startTime = roundStartTime currRound
    let location = roundLocation currRound
    let volume = roundVolume currRound
    let brightness = roundBrightness currRound
    let player1Score = (score location (Head (playerSlot0 player1))) 
                     + (score location (Body (playerSlot1 player1)))
                     + (score location (Leg  (playerSlot2 player1)))
    let player2Score = (score location (Head (playerSlot0 player2))) 
                     + (score location (Body (playerSlot1 player2)))
                     + (score location (Leg  (playerSlot2 player2)))
    (phase, secondsRemaining) <- liftIO $ phaseAndTimeForStartTime startTime
    let state = GameState { currentPhase = phase
                          , timeSincePhaseStart = secondsRemaining
                          , player1 = player1
                          , player2 = player2
                          , location = location
                          , winner = if player1Score == player2Score then Tie
                                     else if player1Score > player2Score then Player1
                                     else Player2
                          , settings = GameSettings 
                            { volume = volume
                            , brightness = brightness
                            }
                          }
    return $ toJSON state
