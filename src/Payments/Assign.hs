{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Payments.Assign where

import Import

import Text.Read (readMaybe)

import Database.Persist.Sql
import qualified Data.List as L
import qualified Data.Map.Strict as Map

type PayIdMap = Map Int UserId

getPaymentsIdMap :: DB PayIdMap
getPaymentsIdMap = do
    users <- selectList [UserPaymentsId !=. Nothing] []
    return $ Map.fromList $ L.map (\(Entity uid u) -> (maybe (-1) Prelude.id $ userPaymentsId u, uid)) users

assign :: PayIdMap -> Payment -> Maybe (UserId, Text)
assign payIdMap p
        | (paymentAmount p) < 0 = Nothing
        | otherwise             = case parseNum >>= lookupId of
            Just uid -> Just (uid, "payments id")
            Nothing  -> Nothing  -- No account matching - only assign by payment ID
  where
    parseNum   = readMaybe $ unpack $ paymentIdentification p :: Maybe Int
    lookupId n = Map.lookup n payIdMap

assignMany :: PayIdMap -> [Payment] -> [(Payment, Maybe Text)]
assignMany payIdMap ps = L.map f ps
  where
    f p = case assign payIdMap p of
            Nothing         -> (p, Nothing)
            Just (uid, how) -> (p { paymentUser = Just uid }, Just how)
