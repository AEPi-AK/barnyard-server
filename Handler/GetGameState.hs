module Handler.GetGameState where

import Import
import DB
import Data.GameState

getGetGameStateR :: Handler Value
getGetGameStateR = do 
    (player1, player2) <- getPlayers
    startTime <- getRoundStartTime
    (phase, secondsRemaining) <- liftIO $ phaseAndTimeForStartTime startTime
    location <- getRoundLocation
    let state = GameState { currentPhase = phase
                          , timeSincePhaseStart = secondsRemaining
                          , player1 = player1
                          , player2 = player2
                          , location = location
                          }
    return $ toJSON state
