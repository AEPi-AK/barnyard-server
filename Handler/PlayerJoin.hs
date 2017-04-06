module Handler.PlayerJoin where

import Import
import DB
import Data.GameState

postPlayerJoinR :: PlayerId -> Handler Value
postPlayerJoinR playerId = do
    startTime <- getRoundStartTime
    (phase, _time) <- liftIO $ phaseAndTimeForStartTime startTime
    _ <- if phase == GameWaiting then startNewRound else return ()
    addPlayer playerId
