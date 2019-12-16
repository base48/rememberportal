{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}

import Prelude     (IO, head)
import Application (handler, cmdLog)
import Import
import Mail
import Handler.Payments

import System.Exit (die)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map

main = handler $ do
    as <- getYesod >>= return . appSettings
    when (isNothing $ appRoot as) $ liftIO $ die "Reminder emails need approot to be configured"
    render <- getUrlRender
    (members, levelMap) <- runDB $ do
        d <- debtors
        levels <- selectList ([] :: [Filter Level]) []
        let l = Map.fromList $ map (\(Entity lid l) -> (lid, l)) levels
        return (d, l)
    forM members $ \((Entity mid m), balance) -> do
        let moa = monthlyAmount as levelMap m
        when (moa + balance < 0) $ liftIO $ do
            let paymentsUrl = render $ PaymentsMemberR mid
            result :: Either SomeException () <- try $ sendReminder as m balance moa paymentsUrl
            case result of
                Left e  -> cmdLog $ "Failed to send email to " <> userEmail m <> ": " <> tshow e
                Right _ -> cmdLog $ "Reminder sent to " <> userEmail m
            threadDelay 1000000 -- XXX GHC only

monthlyAmount :: AppSettings -> Map LevelId Level -> User -> Rational
monthlyAmount as levelMap m = case userLevel m of
        Nothing -> (0 - 666)
        Just ul -> case Map.lookup  ul levelMap of
                Nothing -> (0 - 666)
                Just l  -> fst $ actualFee (appFlexibleFees as) m l
