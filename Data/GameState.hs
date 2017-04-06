module Data.GameState where

import Import
import Data.Time.Clock

data GameState = GameState { currentPhase :: GamePhase
                           , timeSincePhaseStart :: NominalDiffTime
                           , player1 :: Player
                           , player2 :: Player
                        } deriving (Show, Eq)

instance ToJSON GameState where
    toJSON state = (object 
      [ "currentPhase"        .= (show $ currentPhase state)
      , "timeSincePhaseStart" .= (show $ timeSincePhaseStart state)
      , "player1"               .= (toJSON $ player1 state)
      , "player2"               .= (toJSON $ player2 state)
      ])

data GamePhase = GameWaiting | GameInProgress | GameOver
    deriving(Show, Read, Eq)

phaseForTimeDiff :: NominalDiffTime -> GamePhase
phaseForTimeDiff diff | diff > 45 = GameWaiting
                      | diff > 30 = GameOver
                      | diff >= 0 = GameInProgress
                        
phaseAndTimeForStartTime :: UTCTime -> IO (GamePhase, NominalDiffTime)
phaseAndTimeForStartTime time = do
    currTime <- getCurrentTime
    let remSecs = diffUTCTime currTime time
    return (phaseForTimeDiff remSecs, remSecs)
