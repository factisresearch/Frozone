{-# LANGUAGE OverloadedStrings #-}
module Frozone.Util.Email where

import Frozone.Types
import Frozone.Util.Logging

import Network.Mail.SMTP
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

sendEmail :: Maybe FrozoneSmtp -> T.Text -> [T.Text] -> T.Text -> T.Text -> IO ()
sendEmail mSmtp from to subject message =
    case mSmtp of
      Nothing ->
          doLog LogWarn ("No SMTP server configured. Won't send email " ++ show subject)
      Just smtp ->
          let sendFun =
                  case (fs_user smtp, fs_password smtp) of
                    (Just suser, Just spass) ->
                        sendMailWithLogin' (fs_host smtp) (fromIntegral $ fs_port smtp) suser spass
                    _ ->
                        sendMail' (fs_host smtp) (fromIntegral $ fs_port smtp)
          in sendFun $ simpleMail from' to' [] [] subject [plainTextPart $ TL.fromStrict message]
      where
        from' = Address (Just "Frozone") from
        to' = map (Address Nothing) to
