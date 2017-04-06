module Data.GameState where

import Import

data GameState = GameState { currentPhase :: GamePhase
                           , timeSincePhaseStart :: UTCTime
                           , player1 :: Player
                           , player2 :: Player
                        } deriving (Show, Read, Eq)

instance ToJSON GameState where
    toJSON state = (object 
      [ "currentPhase"        .= (show $ currentPhase state)
      , "timeSincePhaseStart" .= (show $ timeSincePhaseStart state)
      , "player1"               .= (toJSON $ player1 state)
      , "player2"               .= (toJSON $ player2 state)
      ])

data GamePhase = GameWaiting | GameInProgress | GameOver
    deriving(Show, Read, Eq)
