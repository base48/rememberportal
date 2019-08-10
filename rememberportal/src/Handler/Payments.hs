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
getPaymentsMemberR uid = do
    defaultLayout $ do
        setTitle . toHtml $ ("Payments for FIXME" :: Text)
        error "TBD"
        -- $(widgetFile "payments") -- FIXME

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
    currency <- liftHandler $ getYesod >>= return . appCurrency . appSettings
    [whamlet|
      <tr>
        <td>#{show $ utctDay $ paymentDate p}
        <td>#{paymentKind p}
        <td>#{paymentLocalAccount p}
        <td>#{paymentRemoteAccount p}
        <td style="text-align:right">
          $if amount > 0
            <span .label .label-success>#{showRational amount} #{currency}
          $else
            <span .label .label-important>#{showRational amount} #{currency}
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
    username uid = fromMaybe "UNKNOWN ID" $ Map.lookup uid userMap

getUserList :: Handler [Entity User]
getUserList = do
    members <- runDB $ selectList ([] :: [Filter User]) []
    return members

getUserMap :: Handler (Map UserId Text)
getUserMap = do
    members <- getUserList
    return $ Map.fromList $ map (\(Entity uid u) -> (uid, userIdent u)) members

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
