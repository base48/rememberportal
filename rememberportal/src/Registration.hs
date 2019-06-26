{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Registration where

import Import.NoFoundation

import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import qualified Data.Text.Lazy.Encoding as TLE

-- uses sendmail executable w/ default options
sendRegEmail :: AppSettings -> Text -> Text -> IO ()
sendRegEmail as email verurl = renderSendMailCustom sendmailBin sendmailOpts $ (emptyMail fromAddr)
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart]]
            }
      where
        sendmailBin = appMailSendmailBin as
        sendmailOpts = ["-t"]
        fromAddr = Address (Just "memberportal") (appMailFrom as)
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partHeaders = []
            , partContent = TLE.encodeUtf8
                [stext|
Please confirm your email address by clicking on the link below.

#{verurl}

Thank you|]
            }
