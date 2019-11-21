{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Payments.Assign where

import Import

import Text.Read (readMaybe)

import Database.Persist.Sql
import qualified Data.List as L
import qualified Data.Map.Strict as Map

type AcctMap  = Map Text UserId
type PayIdMap = Map Int UserId

getRemoteAcctMap :: Text -> DB AcctMap
getRemoteAcctMap kind = do
    pairs <- query
    return $ L.foldl' f Map.empty pairs
  where
    query :: DB [(UserId, Single Text)]
    query = rawSql "SELECT DISTINCT user, remote_account \
                   \FROM payment \
                   \WHERE kind=? \
                     \AND user IS NOT NULL \
                     \AND remote_account <> '' \
                     \AND amount > 0 \
                   \ORDER BY user, remote_account"
                   [PersistText kind]
    f origMap (uid, Single acct) = updateAcctMap origMap acct uid

updateAcctMap :: AcctMap -> Text -> UserId -> AcctMap
updateAcctMap origMap acct uid = case Map.lookup acct origMap of
            Nothing                       -> Map.insert acct uid origMap
            Just someUid | uid == someUid -> origMap
                         | otherwise      -> Map.delete acct origMap

getPaymentsIdMap :: DB PayIdMap
getPaymentsIdMap = do
    users <- selectList [UserPaymentsId !=. Nothing] []
    return $ Map.fromList $ L.map (\(Entity uid u) -> (maybe (-1) Prelude.id $ userPaymentsId u, uid)) users

assign :: PayIdMap -> AcctMap -> Payment -> Maybe (UserId, Text, AcctMap)
assign payIdMap acctMap p = case parseNum >>= lookupId of
        Just uid -> Just (uid, "payments id", acctMap)
        Nothing  -> wrap <$> lookupAcct
  where
    acct       = paymentRemoteAccount p
    parseNum   = readMaybe $ unpack $ paymentIdentification p :: Maybe Int
    lookupId n = Map.lookup n payIdMap
    lookupAcct = Map.lookup acct acctMap
    wrap uid   = (uid, "account number", updateAcctMap acctMap acct uid)

assignMany :: PayIdMap -> AcctMap -> [Payment] -> [(Payment, Maybe Text)]
assignMany payIdMap acctMap ps = L.reverse $ snd $ L.foldl' f (acctMap, []) ps
  where
    f (origMap, result) p = case assign payIdMap origMap p of
            Nothing                 -> (origMap, (p, Nothing):result)
            Just (uid, how, newMap) -> (newMap, (p { paymentUser = Just uid }, Just how):result)
