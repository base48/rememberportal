{-# LANGUAGE OverloadedStrings #-}

import Prelude     (IO, head)
import Application (handler, cmdLog)
import Import
import Payments.Fio

-- wreq depends on lens which yesod tries to avoid by using microlens
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.Wreq as Wreq
import qualified Data.Map.Strict as Map

url :: Text -> String
url tok = "https://www.fio.cz/ib_api/rest/last/" ++ (unpack tok) ++ "/transactions.json"

parse :: Text -> Value -> Either Text Payment
parse ourFull tx = case fromJSON tx :: Result FioPayment of
        Error str -> Left $ pack str
        Success a -> toPayment ourFull tx

getPaymentList :: Text -> IO [Payment]
getPaymentList token = do
    r <- Wreq.get (url token)

    let Just accountStatement = r ^? Wreq.responseBody . key "accountStatement"
    let lasttx = accountStatement ^.. key "transactionList" . key "transaction" . values

    let Just ourAcct = accountStatement ^? key "info" . key "accountId" . _String
    let Just ourBank = accountStatement ^? key "info" . key "bankId" . _String
    let ourFull = ourAcct <> "/" <> ourBank

    cmdLog $ "dowloaded: " <> (pack $ show $ length lasttx)

    let parsed = map (parse ourFull) lasttx

    forM (zip parsed lasttx) $ \(ep, val) -> case ep of
            Left err -> cmdLog $ ("failed to parse " <> encodeToStrictText val <> "\nerror: " <> err)
            _        -> return ()

    let ps = rights parsed

    cmdLog $ "parsed: " <> (pack $ show $ length ps)
    return ps

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
    ps <- liftIO $ getPaymentList tok
    is <- runDB $ forM ps $ \p -> do
        res <- insertUnique p
        return $ isJust res
    let inserted = length $ filter id $ is
    let duplicate = length $ filter not $ is
    liftIO $ do
        cmdLog $ "duplicate: " <> (pack $ show $ duplicate)
        cmdLog $ "inserted: " <> (pack $ show $ inserted)
