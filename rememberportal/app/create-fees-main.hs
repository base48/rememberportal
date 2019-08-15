{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import Prelude     (IO)
import Application (handler)
import Import
import qualified Data.Map.Strict as Map

main :: IO ()
main = handler $ do
    ts <- liftIO $ getCurrentTime
    (n, warn) <- runDB $ do
        members <- selectList ([UserState ==. Accepted] ||. [UserState ==. Awaiting]) []
        lvls <- selectList ([] :: [Filter Level]) []
        let matched = match members lvls ts
        let fees = rights matched
        insertMany_ fees
        return (length fees, lefts matched)
    putStr $ unlines $ warn
    putStrLn $ "inserted " <> (tshow n) <> " fee records"

userFee :: UserId -> Entity Level -> UTCTime -> Fee
userFee memberId (Entity lid l) ts = Fee
        { feeUser        = memberId
        , feeLevel       = lid
        , feePeriodStart = ts
        , feeAmount      = levelAmount l
        }

-- match :: [Entity User] -> [Entity Level] -> ([(Entity User, Entity Level)], [Entity User])
match :: [Entity User] -> [Entity Level] -> UTCTime -> [Either Text Fee]
match members lvls ts = map f members
  where
    levelMap = Map.fromList $ map (\(Entity lid l) -> (lid, l)) lvls
    f (Entity memberId m) = case userLevel m of
            Nothing  -> Left $ userIdent m <> " has no level"
            Just lid -> case Map.lookup lid levelMap of
                     Nothing  -> Left $ userIdent m <> " has invalid level"
                     Just l   -> Right $ userFee memberId (Entity lid l) ts
