{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Payments where

import Import
import Payments.Fio

import Data.Aeson
import Data.Time.Calendar
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BL

-- FIXME paging
getPaymentsR :: Handler Html
getPaymentsR = do
    (_uid, u) <- requireAuthPair
    let isStaff = userStaff u
    payments <- runDB $ selectList ([] :: [Filter Payment]) [Desc PaymentDate]
    userMap <- getUserMap
    let total = sum $ map (\(Entity _ p) -> paymentAmount p) payments -- FIXME breaks with pagination or w/ multiple accounts
    defaultLayout $ do
        setTitle . toHtml $ ("Finance" :: Text)
        $(widgetFile "payments")

getPaymentsMemberR :: UserId -> Handler Html
getPaymentsMemberR memberId = do
    isStaff <- do
        (_uid, u) <- requireAuthPair
        return $ userStaff u
    m <- runDB $ get404 memberId
    payments <- runDB $ selectList [PaymentUser ==. Just memberId] [Desc PaymentDate]
    fees <- runDB $ selectList [FeeUser ==. memberId] [Desc FeePeriodStart]
    let userMap = Map.singleton memberId $ userIdent m
    levelMap <- getLevelMap
    let balance = memberBalance' fees payments
    ((_, addFeesWidget), addFeesEnctype) <- addFeesForm' memberId
    defaultLayout $ do
        setTitle . toHtml $ ("Payments for " <> (tshow $ userIdent m))
        $(widgetFile "payments-member")

getPaymentsEditR :: PaymentId -> Handler Html
getPaymentsEditR pid = do
    p <- runDB $ get404 pid
    userList <- getUserList
    (widget, enctype) <- generateFormPost $ paymentForm userList p
    paymentsEditHelper pid p widget enctype

postPaymentsEditR :: PaymentId -> Handler Html
postPaymentsEditR pid = do
    p <- runDB $ get404 pid
    userList <- getUserList
    ((result, widget), enctype) <- runFormPost $ paymentForm userList p
    case result of
        FormMissing -> do
            addMessage "danger" $ toHtml ("Form missing" :: Text)
            redirect $ PaymentsEditR pid
        FormFailure fs -> do
            forM_ fs (addMessage "danger" . toHtml)
            paymentsEditHelper pid p widget enctype
        FormSuccess np -> do
            runDB $ replace pid np
            addMessage "success" "Payment updated"
            redirect $ PaymentsR

postPaymentsMemberFeeR :: Key User -> Handler Html
postPaymentsMemberFeeR uid = do
    ((result, _widget), _enctype) <- addFeesForm' uid
    case result of
        FormMissing -> do
            addMessage "danger" $ toHtml ("Form missing" :: Text)
            redirect $ PaymentsMemberR uid
        FormFailure fs -> do
            forM_ fs (addMessage "danger" . toHtml)
            redirect $ PaymentsMemberR uid
        FormSuccess (AddFeesData _from _to Nothing) -> do
            addMessage "danger" $ toHtml ("Level missing" :: Text)
            redirect $ PaymentsMemberR uid
        FormSuccess (AddFeesData from to (Just levelId)) -> do
            level <- runDB $ get404 levelId
            let months = takeWhile (<to) $ iterate' nextMonth (firstOfMonth from)
            let fees = map (\ut -> Fee uid levelId ut (levelAmount level)) months
            $(logWarn) (pack $ unlines $ map show fees)
            runDB $ insertMany_ fees
            addMessage "success" "Fee records created"
            redirect $ PaymentsMemberR uid
  where
    onDay f ut = flip UTCTime (utctDayTime ut) $ f $ utctDay ut
    onGregorian f ut = onDay ((\(uty, utm, utd) -> fromGregorian uty utm utd) . f . toGregorian) ut
    firstOfMonth :: UTCTime -> UTCTime
    firstOfMonth ut = onGregorian (\(y, m, _d) -> (y, m, 1)) ut
    nextMonth :: UTCTime -> UTCTime
    nextMonth ut = onDay (addGregorianMonthsClip 1) ut
    iterate' f x = x : iterate' f (f x) -- where the fuck do i import it from?

paymentsEditHelper :: PaymentId -> Payment -> Widget -> Enctype -> Handler Html
paymentsEditHelper pid p widget enctype = do
    userMap <- getUserMap
    defaultLayout $ do
        setTitle . toHtml $ ("Edit payment" :: Text)
        $(widgetFile "payments-edit")

paymentHeader :: Bool -> Widget
paymentHeader isStaff = do
    [whamlet|
      <tr>
        <th>Date
        <th>Kind
        <th>To
        $if isStaff
          <th>From
        <th style="text-align:right">Amount
        <th style="text-align:right">ID
        <th>Details
        <th>Fee for
    |]

paymentRow :: Bool -> Map UserId Text -> Entity Payment -> Widget
paymentRow isStaff userMap (Entity pid p) = do
    let details = if paymentKind p == "fio"
            then fioDetails $ paymentJson p
            else [("kind", "unknown")]
    [whamlet|
      <tr>
        <td>#{show $ utctDay $ paymentDate p}
        <td>#{paymentKind p}
        <td>#{paymentLocalAccount p}
        $if isStaff
          <td>#{paymentRemoteAccount p}
        <td style="text-align:right">
          ^{balanceWidget amount}
        <td style="text-align:right">#{paymentIdentification p}
        <td>
            $forall (k, v) <- details
                <i>#{k}</i>:&nbsp;#{v}
        <td>
          $maybe uid <- paymentUser p
            <a href=@{PaymentsMemberR uid}>#{username uid}
          $nothing
            <i>
          $if isStaff
            &nbsp;[<a href=@{PaymentsEditR pid}>edit</a>]
    |]
  where
    amount = paymentAmount p
    username uid = fromMaybe ("UNKNOWN ID " <> tshow uid) $ Map.lookup uid userMap
    fioDetails jsonText = case eitherDecode $ BL.fromStrict $ TE.encodeUtf8 $ jsonText :: Either String FioPayment of
            Left str  -> [("error", pack str)]
            Right fp  -> descriptionShort fp

feeRow :: Map LevelId Text -> Entity Fee -> Widget
feeRow levelMap (Entity _fid f) = do
    [whamlet|
      <tr>
        <td> #{formatTime defaultTimeLocale "%Y-%m" $ feePeriodStart f}
        <td> #{levelname $ feeLevel f}
        <td style="text-align:right">
          ^{balanceWidget $ negate $ feeAmount f}
    |]
  where
    levelname lid = fromMaybe ("UNKNOWN ID " <> tshow lid) $ Map.lookup lid levelMap

getUserList :: Handler [Entity User]
getUserList = do
    members <- runDB $ selectList ([] :: [Filter User]) []
    return members

getUserMap :: Handler (Map UserId Text)
getUserMap = do
    members <- getUserList
    return $ Map.fromList $ map (\(Entity uid u) -> (uid, userIdent u)) members

getLevelMap :: Handler (Map LevelId Text)
getLevelMap = do
    lvls <- runDB $ selectList ([] :: [Filter Level]) []
    return $ Map.fromList $ map (\(Entity lid l) -> (lid, levelName l)) lvls

paymentForm :: [Entity User] -> Payment -> Form Payment
paymentForm userList p = renderBootstrap2 $ Payment
    <$> aopt userSelect "Member" (Just $ paymentUser p)
    <*> (pure $ paymentDate p)
    <*> (pure $ paymentAmount p)
    <*> (pure $ paymentKind p)
    <*> (pure $ paymentKindId p)
    <*> (pure $ paymentLocalAccount p)
    <*> (pure $ paymentRemoteAccount p)
    <*> (pure $ paymentIdentification p)
    <*> (pure $ paymentJson p)
    <*> (pure $ paymentStaffComment p)
  where
    userSelect = selectFieldList $ map (\(Entity uid u) -> (userIdent u, uid)) userList

data AddFeesData = AddFeesData UTCTime UTCTime (Maybe LevelId)

addFeesForm :: Text -> UTCTime -> [Entity Level] -> User -> Form AddFeesData
addFeesForm currency curTime levels u = renderBootstrap2 $ AddFeesData
    <$> (dayToUTC <$> (areq dayField "From" (Just $ utctDay $ userDateJoined u)))
    <*> (dayToUTC <$> (areq dayField "To" (Just $ utctDay $ curTime)))
    <*> (aopt levelField "Level" $ (Just $ userLevel u))
  where
    dayToUTC day = UTCTime day 0
    levelField = selectFieldList $ map (\(Entity lid lvl) -> (formatLevel lvl "" currency, lid)) levels

addFeesForm' :: Key User -> Handler ((FormResult AddFeesData, Widget), Enctype)
addFeesForm' uid = do
    u <- runDB $ get404 uid
    lvls <- runDB $ selectList [LevelActive ==. True] [Asc LevelAmount]
    currency <- getCurrency
    curTime <- liftIO getCurrentTime
    runFormPost $ addFeesForm currency curTime lvls u

-- XXX optimizable
memberBalance :: UserId -> DB Rational
memberBalance memberId = do
    payments <- selectList [PaymentUser ==. Just memberId] []
    fees <- selectList [FeeUser ==. memberId] []
    return $ memberBalance' fees payments

memberBalance' :: [Entity Fee] -> [Entity Payment] -> Rational
memberBalance' fees payments = total_p - total_f
  where
    total_p = sum $ map (\(Entity _pid p) -> paymentAmount p) payments
    total_f = sum $ map (\(Entity _fid f) -> feeAmount f) fees

getCurrency :: Handler Text
getCurrency = getYesod >>= return . appCurrency . appSettings

currencyWidget :: Widget
currencyWidget = do
    currency <- handlerToWidget $ getCurrency
    [whamlet|#{currency}|]

balanceWidget :: Rational -> Widget
balanceWidget balance = do
    currency <- handlerToWidget $ getCurrency
    let sigcl = if balance >= 0 then "label-success" else "label-important" :: Text
    [whamlet|<span .label class=#{sigcl}>#{showRational balance}&nbsp;#{currency}|]

formatLevel :: Level -> Text -> Text -> Text
formatLevel lvl prefix currency = levelName lvl <> " [" <> prefix <> (showRational $ levelAmount lvl) <> " " <> currency <> "]"

debtors :: DB [(Entity User, Rational)]
debtors = do
    users <- selectList [UserState ==. Accepted] []
    x <- forM users $ \e@(Entity memberId _) -> do
            balance <- memberBalance memberId
            return (e, balance)
    return $ sortOn snd $ filter (\e -> snd e < 0) x
