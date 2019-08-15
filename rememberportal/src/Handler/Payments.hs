{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Payments where

import Import

import qualified Data.Map.Strict as Map

-- FIXME paging
getPaymentsR :: Handler Html
getPaymentsR = do
    (_uid, u) <- requireAuthPair
    let isStaff = userStaff u
    payments <- runDB $ selectList ([] :: [Filter Payment]) []
    userMap <- getUserMap
    defaultLayout $ do
        setTitle . toHtml $ ("Finance" :: Text)
        $(widgetFile "payments")

getPaymentsMemberR :: UserId -> Handler Html
getPaymentsMemberR memberId = do
    isStaff <- do
        (_uid, u) <- requireAuthPair
        return $ userStaff u
    m <- runDB $ get404 memberId
    payments <- runDB $ selectList [PaymentUser ==. Just memberId] []
    fees <- runDB $ selectList [FeeUser ==. memberId] []
    let userMap = Map.singleton memberId $ userIdent m
    levelMap <- getLevelMap
    let balance = memberBalance' fees payments
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

paymentsEditHelper :: PaymentId -> Payment -> Widget -> Enctype -> Handler Html
paymentsEditHelper pid p widget enctype = do
    userMap <- getUserMap
    defaultLayout $ do
        setTitle . toHtml $ ("Edit payment" :: Text)
        $(widgetFile "payments-edit")

paymentRow :: Bool -> Map UserId Text -> Entity Payment -> Widget
paymentRow isStaff userMap (Entity pid p) = do
    [whamlet|
      <tr>
        <td>#{show $ utctDay $ paymentDate p}
        <td>#{paymentKind p}
        <td>#{paymentLocalAccount p}
        <td>#{paymentRemoteAccount p}
        <td style="text-align:right">
          $if amount > 0
            <span .label .label-success>#{showRational amount} ^{currencyWidget}
          $else
            <span .label .label-important>#{showRational amount} ^{currencyWidget}
        <td>#{paymentIdentification p}
        <td>TBD format details
        <td>
          $maybe uid <- paymentUser p
            #{username uid}
          $nothing
            &nbsp;
          $if isStaff
            &nbsp;[<a href=@{PaymentsEditR pid}>edit</a>]
    |]
  where
    amount = paymentAmount p
    username uid = fromMaybe ("UNKNOWN ID " <> tshow uid) $ Map.lookup uid userMap

feeRow :: Map LevelId Text -> Entity Fee -> Widget
feeRow levelMap (Entity _fid f) = do
    [whamlet|
      <tr>
        <td> #{show $ utctDay $ feePeriodStart f}
        <td> #{levelname $ feeLevel f}
        <td style="text-align:right">
          <span .label .label-success>#{showRational $ feeAmount f} ^{currencyWidget}
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
    <*> (pure $ paymentLocalAccount p)
    <*> (pure $ paymentRemoteAccount p)
    <*> (pure $ paymentIdentification p)
    <*> (pure $ paymentJson p)
  where
    userSelect = selectFieldList $ map (\(Entity uid u) -> (userIdent u, uid)) userList

-- XXX optimizable
memberBalance :: UserId -> Handler Rational
memberBalance memberId = do
    payments <- runDB $ selectList [PaymentUser ==. Just memberId] []
    fees <- runDB $ selectList [FeeUser ==. memberId] []
    return $ memberBalance' fees payments

memberBalance' :: [Entity Fee] -> [Entity Payment] -> Rational
memberBalance' fees payments = total_p - total_f
  where
    total_p = sum $ map (\(Entity _pid p) -> paymentAmount p) payments
    total_f = sum $ map (\(Entity _fid f) -> feeAmount f) fees

currencyWidget :: Widget
currencyWidget = do
    currency <- handlerToWidget $ getYesod >>= return . appCurrency . appSettings
    [whamlet|#{currency}|]

balanceWidget :: Rational -> Widget
balanceWidget balance = do
    currency <- handlerToWidget $ getYesod >>= return . appCurrency . appSettings
    let sigcl = if balance > 0 then "label-success" else "label-important" :: Text
    [whamlet|<span .label class=#{sigcl}>#{showRational balance} #{currency}|]
