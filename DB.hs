{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module DB where

import Import
import Database.Persist.Sql

getRoundStartTime :: Handler UTCTime
getRoundStartTime = do
    currRound <- runDB $ get404 $ toSqlKey (fromIntegral (1 :: Int))
    return $ roundStartTime currRound

startNewRound :: Handler ()
startNewRound = do
    currTime <- liftIO $ getCurrentTime
    let (roundId :: RoundId) = toSqlKey (fromIntegral (1 :: Int))
    runDB $ update roundId [RoundStartTime =. currTime] 
    return ()
