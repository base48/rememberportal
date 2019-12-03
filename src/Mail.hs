{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Mail
    ( sendRegEmail
    , sendReminder
    ) where

import Import.NoFoundation

import Network.Mail.Mime
import Text.Shakespeare.Text (stext, textFile)
import qualified Data.Text.Lazy.Encoding as TLE
-- import qualified Data.Text.Encoding as TE
-- import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as L

sendRegEmail :: AppSettings -> Text -> Text -> IO ()
sendRegEmail as email verurl = sendAnEmail as email subj [stext|
Please confirm your email address by clicking on the link below.

#{verurl}

Thank you|]
-- possible to use ltext for lazy text
  where
    subj = "Verify your email address"


-- TODO reply-to: admin/council
sendReminder :: AppSettings -> User -> Rational -> IO ()
sendReminder as user balance = sendAnEmail as (userEmail user) subj [stext|

Hi #{userIdent user}!

Have you paid your #{appOrgName as} membership?

Your balance is: #{showRational balance} #{appCurrency as}

See your account for fees and payments: TODO URL

Payment details:
 Account number: 2900086515/2010
         Amount: #{showRational $ 0 - balance}   (FIXME monthly)
Variable symbol: FIXME

Thank you for supporting #{appOrgName as}!
|]
  where -- asdf
    subj = "Membership fees reminder"


sendAnEmail :: AppSettings -> Text -> Text -> L.Text -> IO ()
sendAnEmail as email subject body = renderSendMailCustom sendmailBin sendmailOpts $ (emptyMail fromAddr)
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", subject)
                ]
            , mailParts = [[textPart]]
            }
      where
        sendmailBin = appMailSendmailBin as
        sendmailOpts = ["-t"]
        fromAddr = Address (Just $ appOrgName as) (appMailFrom as)
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partDisposition = DefaultDisposition
            , partHeaders = []
            , partContent = PartContent $ TLE.encodeUtf8 $ body
            }

-- ffuuuuuuuuuu
-- FIXME utils
showRational :: Rational -> Text
showRational x = pack $ show $ (fromRational x :: Double)
