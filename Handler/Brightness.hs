module Handler.Brightness where

import Import
import DB

postBrightnessR :: Int -> Handler Value
postBrightnessR brightness = do
    _ <- updateBrightness brightness
    return $ object ["status" .= ("success" :: Text)]
