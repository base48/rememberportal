{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Mail
    ( sendRegEmail
    , sendReminder
    ) where

import Import.NoFoundation

import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
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


sendReminder :: AppSettings -> User -> Rational -> Rational -> Text -> IO ()
sendReminder as user balance monthlyAmount paymentsUrl  = sendAnEmail as (userEmail user) subj [stext|
Hi #{userIdent user}!

Have you paid your #{appOrgName as} membership?

Your balance is: #{showRational balance} #{appCurrency as}

See your account for fees and payments: #{paymentsUrl}

Payment details:
Account number:     #{fromMaybe "ERROR" (appFeeAccount as)}
Variable symbol:    #{show $ fromMaybe (0 - 1337) $ userPaymentsId user}
Outstanding amount: #{showRational $ 0 - balance} #{appCurrency as}
Monthly amount:     #{showRational monthlyAmount} #{appCurrency as}

Thank you for supporting #{appOrgName as}!
|]
  where
    subj = "Membership fees reminder"


sendAnEmail :: AppSettings -> Text -> Text -> L.Text -> IO ()
sendAnEmail as email subject body = renderSendMailCustom sendmailBin sendmailOpts $ (emptyMail fromAddr)
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", subject)
                ] <> replyTo
            , mailParts = [[textPart]]
            }
      where
        replyTo = case appMailReplyTo as of
                Nothing -> []
                Just r  -> [("Reply-To", r)]
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
