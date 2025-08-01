{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Payments.Fio
    ( PaymentSource(..)
    , FioPayment
    , toPayment
    , encodeToStrictText -- FIXME move to utils
    ) where

-- https://www.fio.cz/docs/cz/API_Bankovnictvi.pdf

import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Text
import Data.Aeson.Types
import Data.Scientific
import Data.Text
import Data.Time.Clock
import Data.Time.Format

import qualified Data.Text.Lazy as L

import Model

class PaymentSource a where
  kind             :: a -> Text
  descriptionShort :: a -> [(Text, Text)]
  descriptionLong  :: a -> [(Text, Text)]

data FioPayment = FioPayment
    { fioID             :: Maybe Int      -- 22
    , fioDate           :: Maybe UTCTime  -- 0 formatted as yyyy-mm-dd+nnnn
    , fioAmount         :: Maybe Rational -- 1
    , fioCurrency       :: Maybe Text     -- 14 ISO4217
    , fioRemoteAcctNum  :: Maybe Text     -- 2
    , fioRemoteAcctName :: Maybe Text     -- 10
    , fioRemoteBankNum  :: Maybe Text     -- 3
    , fioRemoteBankName :: Maybe Text     -- 12
    , fioConstantSym    :: Maybe Text     -- 4
    , fioVariableSym    :: Maybe Text     -- 5
    , fioSpecificSym    :: Maybe Text     -- 6
    , fioUserIdent      :: Maybe Text     -- 7
    , fioMessage        :: Maybe Text     -- 16
    , fioType           :: Maybe Text     -- 8
    , fioAuthorizedBy   :: Maybe Text     -- 9
    , fioSpecification  :: Maybe Text     -- 18
    , fioComment        :: Maybe Text     -- 25
    , fioBIC            :: Maybe Text     -- 26 ISO9362
    , fioCommandID      :: Maybe Int      -- 17
    }

instance FromJSON FioPayment where
    parseJSON = withObject "FioPayment" $ \v -> do
        fioID             <- parseColumn 22 v
        fioAmount'        <- parseColumn 1 v :: Parser (Maybe Scientific)
        let fioAmount = fmap toRational fioAmount'
        fioDate'          <- parseColumn 0 v :: Parser (Maybe String)
        let fioDate = fmap convertTime fioDate'
        -- let x = epicfail parseColumn
        fioCurrency       <- parseColumn 14 v
        fioRemoteAcctNum  <- parseColumn 2  v
        fioRemoteAcctName <- parseColumn 10 v
        fioRemoteBankNum  <- parseColumn 3  v
        fioRemoteBankName <- parseColumn 12 v
        fioConstantSym    <- parseColumn 4  v
        fioVariableSym    <- parseColumn 5  v
        fioSpecificSym    <- parseColumn 6  v
        fioUserIdent      <- parseColumn 7  v
        fioMessage        <- parseColumn 16 v
        fioType           <- parseColumn 8  v
        fioAuthorizedBy   <- parseColumn 9  v
        fioSpecification  <- parseColumn 18 v
        fioComment        <- parseColumn 25 v
        fioBIC            <- parseColumn 26 v
        fioCommandID      <- parseColumn 17 v
        return FioPayment{..}
      where
        parseColumn :: FromJSON a => Int -> Object -> Parser (Maybe a)
        parseColumn num v = do
            mOuter <- v .:? ("column" <> (fromText $ tshow num))
            case mOuter of
                Nothing -> return Nothing
                Just o  -> do
                    mValue <- o .:? "value"
                    return mValue
        convertTime :: String -> UTCTime
        convertTime s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d%z" s

instance PaymentSource FioPayment where
    kind _ = "fio"

    -- XXX maybe some sort of deduplication?
    descriptionShort fp =
         ( opt fioRemoteAcctName "from"
        ++ opt fioRemoteBankName "bank"
        -- ++ opt fioConstantSym    "csym"
        ++ opt fioVariableSym    "vsym"
        -- ++ opt fioSpecificSym    "ssym"
        -- ++ opt fioUserIdent      "user" -- seems to be always redundant
        ++ opt fioMessage        "msg"
        ++ opt fioType           "type"
        ++ opt fioAuthorizedBy   "auth"
        ++ opt fioSpecification  "desc"
        ++ opt fioComment        "comment"
        ++ opt fioBIC            "bic"
         )
      where
        opt getter label = case getter fp of
                Nothing -> []
                Just t  -> [(label, t)]

    descriptionLong fp =
      [ opt fioID       "Payment ID" tshow
      , opt fioAmount   "Amount"     showRational
      , opt fioDate     "Date"       showDate
      , opt fioCurrency "Currency"   id
      , ("From", acct (fioRemoteAcctNum fp) (fioRemoteBankNum fp))
      , opt fioRemoteAcctName "Holder"              id
      , opt fioRemoteBankName "Bank"                id
      , opt fioVariableSym    "Variable symbol"     id
      , opt fioConstantSym    "Constant symbol"     id
      , opt fioSpecificSym    "Specific symbol"     id
      , opt fioUserIdent      "User identification" id
      , opt fioMessage        "Message"             id
      , opt fioType           "Payment type"        id
      , opt fioAuthorizedBy   "Authorized by"       id
      , opt fioSpecification  "Specification"       id
      , opt fioComment        "Comment"             id
      , opt fioBIC            "BIC"                 id
      , opt fioCommandID      "Payment command ID"  tshow
      ]
      where
        opt getter label fmt = case getter fp of
                Nothing -> (label, "-")
                Just x  -> (label, fmt x)
        acct ma mb = case (ma, mb) of
                (Just a, Just b) -> a <> "/" <> b
                _                -> "-"

toPayment :: Text -> Value -> Either Text Payment
toPayment localAcct val = do
        fp <- case fromJSON val of
                Error str  -> Left $ pack str
                Success fp -> return fp
        date <- maybeToEither "Date missing" $ fioDate fp
        fioId <- fmap (pack.show) $ maybeToEither "Payment ID missing" $ fioID fp
        amount <- maybeToEither "Amount missing" $ fioAmount fp
        let remote = case (fioRemoteAcctNum fp, fioRemoteBankNum fp) of
                (Just a, Just b) -> a <> "/" <> b
                _                -> ""
        let jsonText = encodeToStrictText val
        let ident = maybe "" id $ fioVariableSym fp
        let k = kind (undefined :: FioPayment)
        return $ Payment Nothing date amount k fioId localAcct remote ident jsonText ""

-- FIXME utils
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err Nothing  = Left err
maybeToEither _ (Just val) = Right val

-- FIXME utils
tshow :: Show a => a -> Text
tshow = pack . show

-- FIXME utils
showRational :: Rational -> Text
showRational x = pack $ show $ (fromRational x :: Double)

-- FIXME utils
showDate :: UTCTime -> Text
showDate d = pack $ formatTime defaultTimeLocale "%Y-%m-%d" d

encodeToStrictText :: Value -> Text
encodeToStrictText = L.toStrict . encodeToLazyText
