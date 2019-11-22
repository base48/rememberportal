{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.MembersEdit where

import Import

import Data.Char (isDigit)
import Handler.Payments

import qualified Data.List as L

profileEditHelper :: UserId -> Widget -> Enctype -> Handler Html
profileEditHelper memberId widget enctype = defaultLayout $ do
        setTitle . toHtml $ ("Edit profile" :: Text)
        $(widgetFile "profile-edit")

getMemberEditR :: UserId -> Handler Html
getMemberEditR memberId = do
    form <- memberEditForm memberId
    (widget, enctype) <- generateFormPost form
    profileEditHelper memberId widget enctype

postMemberEditR :: UserId -> Handler Html
postMemberEditR memberId = do
    (uid, _u) <- requireAuthPair
    form <- memberEditForm memberId
    ((result, widget), enctype) <- runFormPost form
    -- $logDebug (pack $ show result)
    case result of
        FormMissing -> do
            addMessage "danger" $ toHtml ("Form missing" :: Text)
            redirect $ MemberEditR memberId
        FormFailure fs -> do
            forM_ fs (addMessage "danger" . toHtml)
            profileEditHelper memberId widget enctype
        FormSuccess nu -> do
            ok <- validateFlexibleFees nu
            if ok then do
                runDB $ replace memberId nu
                addMessage "success" "Profile updated"
                redirect $ if uid == memberId then MemberProfileR else MembersOverviewR
            else do
                addMessage "danger" $ toHtml ("Fee can't be less than level minimum" :: Text)
                redirect $ MemberEditR memberId
  where
    validateFlexibleFees :: User -> Handler Bool
    validateFlexibleFees nu = do
        isFlexible <- getYesod >>= return . appFlexibleFees . appSettings
        case userLevel nu of
            Nothing  -> return True
            Just lid -> do
                l <- runDB $ get404 lid
                return ((not isFlexible) || (userLevelActualAmount nu) >= (levelAmount l))

memberEditForm :: UserId -> Handler (Form User)
memberEditForm memberId = do
    (_uid, u) <- requireAuthPair
    master <- getYesod
    let currency = appCurrency $ appSettings master
    let isFlexible = appFlexibleFees $ appSettings master
    (memberData, levels) <- runDB $ do
        m <- get404 memberId
        l <- selectList [LevelActive ==. True] [Asc LevelAmount]
        return (m, l)
    return $ if userStaff u
        then memberEditFormStaff' isFlexible levels currency memberData
        else memberEditForm' isFlexible levels currency memberData

memberEditForm' :: Bool -> [Entity Level] -> Text -> User -> Form User
memberEditForm' isFlexible levels currency u = renderBootstrap2 $ User
    <$> pure (userIdent u)
    <*> areq emailField "Email" (Just $ userEmail u)
    <*> pure (userPassword u) -- password
    <*> pure (userVerkey u) -- verkey
    <*> pure (userVerified u)
    <*> aopt textField "Full name" (Just $ userRealname u)
    <*> aopt textField "Alternative nick" (Just $ userAltnick u)
    <*> aopt phoneField "Phone number" (Just $ userPhone u)
    <*> aopt textField "Alternative contact" (Just $ userAltcontact u)
    <*> pure (userLevel u)
    <*> (if isFlexible
            then (areq amountField (fromString $ unpack flexibleLabel) (Just $ userLevelActualAmount u))
            else pure (userLevelActualAmount u))
    <*> pure (userPaymentsId u)
    -- FIXME why the FUCK is dayField using month/day/year format ...
    <*> pure (userDateJoined u)
    <*> pure (userKeysGranted u)
    <*> pure (userKeysReturned u)
    <*> pure (userState u)
    <*> pure (userCouncil u)
    <*> pure (userStaff u)
  where
    phoneField = strField 4 20 (\c -> isDigit c || c `elem` [' ', '+', '(', ')']) "Phone number" -- XXX DRY
    flexibleLabel = "Membership fee >= " <> (showRational levelFee) <> " " <> currency
    levelFee = case userLevel u of
        Nothing  -> 0
        Just ulid -> levelAmount $ L.head [ l | (Entity lid l) <- levels, ulid == lid ]

memberEditFormStaff' :: Bool -> [Entity Level] -> Text -> User -> Form User
memberEditFormStaff' isFlexible levels currency u = renderBootstrap2 $ User
    <$> areq textField "Nick" (Just $ userIdent u)
    <*> areq emailField "Email" (Just $ userEmail u)
    <*> pure (userPassword u)
    <*> pure (userVerkey u)
    <*> areq checkBoxField "Verified" (Just $ userVerified u)
    <*> aopt textField "Full name" (Just $ userRealname u)
    <*> aopt textField "Alternative nick" (Just $ userAltnick u)
    <*> aopt phoneField "Phone number" (Just $ userPhone u)
    <*> aopt textField "Alternative contact" (Just $ userAltcontact u)
    <*> aopt levelField "Membership level" (Just $ userLevel u)
    <*> (if isFlexible
            then (areq amountField "Membership fee" (Just $ userLevelActualAmount u))
            else pure (userLevelActualAmount u))
    <*> aopt intField "Payments ID" (Just $ userPaymentsId u)
    <*> (dayToUTC <$> areq dayField "Date joined" (Just $ utctDay $ userDateJoined u))
    <*> (fmap dayToUTC <$> aopt dayField "Keys granted" (Just $ utctDay <$> userKeysGranted u))
    <*> (fmap dayToUTC <$> aopt dayField "Keys returned" (Just $ utctDay <$> userKeysReturned u))
    <*> areq (selectField optionsEnum) "State" (Just $ userState u)
    <*> areq checkBoxField "Council" (Just $ userCouncil u)
    <*> areq checkBoxField "Staff" (Just $ userStaff u)
  where
    phoneField = strField 4 20 (\c -> isDigit c || c `elem` [' ', '+', '(', ')']) "Phone number" -- XXX DRY
    dayToUTC day = UTCTime day 0
    levelField = selectFieldList $ map (\(Entity lid lvl) -> (formatLevel lvl pfx currency, lid)) levels
    pfx = if isFlexible then ">= " else ""
