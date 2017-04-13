{-# Language TemplateHaskell #-}
module Handler.GetGameState where

import Import
import DB
import Data.GameState

getGetGameStateR :: Handler Value
getGetGameStateR = do 
    (player1, player2) <- getPlayers
    currRound <- getRound
    let startTime = roundStartTime currRound
    let location = roundLocation currRound
    let volume = roundVolume currRound
    let brightness = roundBrightness currRound
    (phase, secondsRemaining) <- liftIO $ phaseAndTimeForStartTime startTime
    let state = GameState { currentPhase = phase
                          , timeSincePhaseStart = secondsRemaining
                          , player1 = player1
                          , player2 = player2
                          , location = location
                          , winner = Player1
                          , settings = GameSettings 
                            { volume = volume
                            , brightness = brightness
                            }
                          }
    return $ toJSON state
