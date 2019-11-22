{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import Prelude     (IO)
import Application (handler, cmdLog)
import Import
import qualified Data.Map.Strict as Map

main :: IO ()
main = handler $ do
    ts <- liftIO $ getCurrentTime
    flexible <- getYesod >>= return . appFlexibleFees . appSettings
    (n, warn) <- runDB $ do
        members <- selectList ([UserState ==. Accepted] ||. [UserState ==. Awaiting]) []
        lvls <- selectList ([] :: [Filter Level]) []
        let matched = match members lvls ts flexible
        let fees = rights matched
        insertMany_ fees
        return (length fees, lefts matched)
    liftIO $ cmdLog $ unlines $ warn
    liftIO $ cmdLog $ "inserted " <> (tshow n) <> " fee records"

userFee :: Entity User -> Entity Level -> UTCTime -> Bool -> Fee
userFee (Entity memberId m) (Entity lid l) ts flexible = Fee
        { feeUser        = memberId
        , feeLevel       = lid
        , feePeriodStart = ts
        , feeAmount      = fst $ actualFee flexible m l
        }

match :: [Entity User] -> [Entity Level] -> UTCTime -> Bool -> [Either Text Fee]
match members lvls ts flexible = map f members
  where
    levelMap = Map.fromList $ map (\(Entity lid l) -> (lid, l)) lvls
    f (Entity memberId m) = case userLevel m of
            Nothing  -> Left $ userIdent m <> " has no level"
            Just lid -> case Map.lookup lid levelMap of
                     Nothing  -> Left $ userIdent m <> " has invalid level"
                     Just l   -> Right $ userFee (Entity memberId m) (Entity lid l) ts flexible
