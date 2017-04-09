module Handler.Volume where

import Import
import DB

postVolumeR :: Int -> Handler Value 
postVolumeR volume = do
    _ <- updateVolume volume
    return $ object ["status" .= ("success" :: Text)]
