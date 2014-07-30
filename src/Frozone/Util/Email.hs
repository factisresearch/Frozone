module Frozone.Util.Email where

import Network.Email.Sendmail
import qualified Data.Text as T

sendEmail :: T.Text -> [T.Text] -> T.Text -> T.Text -> IO ()
sendEmail from to subject message =
    sendmail (Just $ T.unpack from) (map T.unpack to)
                 ("Subject: Frozone: " ++ (T.unpack subject) ++ "\r\n" ++ (T.unpack message))
