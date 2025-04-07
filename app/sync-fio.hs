{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

import Prelude     (IO, head)
import Application (handler, cmdLog)
import Import
import Payments.Fio
import Payments.Assign

-- wreq depends on lens which yesod tries to avoid by using microlens
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.Wreq as Wreq

url :: Text -> String
url tok = "https://fioapi.fio.cz/ib_api/rest/last/" ++ (unpack tok) ++ "/transactions.json"

parse :: Text -> Value -> Either Text Payment
parse ourFull tx = case fromJSON tx :: Result FioPayment of
        Error str -> Left $ pack str
        Success a -> toPayment ourFull tx

getPaymentList :: Text -> IO ([Payment], Int)
getPaymentList token = do
    r <- Wreq.get (url token)

    let Just accountStatement = r ^? Wreq.responseBody . key "accountStatement"
    let lasttx = accountStatement ^.. key "transactionList" . key "transaction" . values

    let Just ourAcct = accountStatement ^? key "info" . key "accountId" . _String
    let Just ourBank = accountStatement ^? key "info" . key "bankId" . _String
    let ourFull = ourAcct <> "/" <> ourBank

    let nDownloaded = length lasttx
    let parsed = map (parse ourFull) lasttx

    forM (zip parsed lasttx) $ \(ep, val) -> case ep of
            Left err -> cmdLog $ ("failed to parse " <> encodeToStrictText val <> "\nerror: " <> err)
            _        -> return ()

    let ps = rights parsed

    return (ps, nDownloaded)

getToken :: Maybe FilePath -> IO Text
getToken Nothing     = error "No token configured"
getToken (Just [])   = error "No token configured"
getToken (Just path) = do
    f <- T.readFile path
    return $ T.strip f

main :: IO ()
main = handler $ do
    mTokenPath <- getYesod >>= return . appFioTokenPath . appSettings
    tok <- liftIO $ getToken mTokenPath
    (ps, nDownloaded) <- liftIO $ getPaymentList tok
    let nParsed = length ps
    is <- runDB $ do
        acctMap <- getRemoteAcctMap "fio"
        payIdMap <- getPaymentsIdMap
        let assigned = assignMany payIdMap acctMap ps
        forM assigned $ \(p, mhow) -> do
            when (isJust mhow) $ liftIO $ cmdLog $ "assigned by " <> (fromJust mhow) <> ": " <> (pack $ show p)
            res <- insertUnique p
            return $ (isJust res, mhow)
    let nInserted = countNum fst is
    let nId = countMatch "payments id" is
    let nAcct = countMatch "account number" is
    let nDuplicate = nParsed - nInserted
    let stats =
            [ ("downloaded", nDownloaded)
            , ("parsed",     nParsed)
            , ("duplicate",  nDuplicate)
            , ("inserted",   nInserted)
            , ("assigned by id", nId)
            , ("assigned by account", nAcct)
            ]
    liftIO $ forM_ stats $ \(s, n) -> do
        cmdLog $ s <> ": " <> (pack $ show $ n)

countNum p xs = length $ filter p xs

countMatch :: Text -> [(Bool, Maybe Text)] -> Int
countMatch s xs = countNum f xs
  where
    f (True, Just how) = how == s
    f _                = False
