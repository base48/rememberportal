{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Members where

import Import

import Data.Char (isDigit)

getMembersOverviewR :: Handler Html
getMembersOverviewR = do
    num_total <- runDB $ count [UserState ==. Accepted]
    num_awaiting <- runDB $ count [UserState ==. Awaiting]
    num_ex <- runDB $ count [UserState ==. Exmember]
    num_rejected <- runDB $ count [UserState ==. Rejected]
    defaultLayout $ do
        setTitle . toHtml $ ("Members overview" :: Text)
        $(widgetFile "members-overview")
  where
    num_paying = 1337 :: Int -- FIXME
    num_nonpaying = 666 :: Int

getMembersAcceptedR :: Handler Html
getMembersAcceptedR = membersList [UserState ==. Accepted]

getMembersRejectedR :: Handler Html
getMembersRejectedR = membersList [UserState ==. Rejected]

getMembersAwaitingR :: Handler Html
getMembersAwaitingR = membersList [UserState ==. Awaiting]

getMembersExR :: Handler Html
getMembersExR = membersList [UserState ==. Exmember]

getMembersLevelR :: LevelId -> Handler Html
getMembersLevelR lid = membersList [UserLevel ==. Just lid]

getMembersNoLevelR :: Handler Html
getMembersNoLevelR = membersList [UserLevel ==. Nothing]

membersList :: [Filter User] -> Handler Html
membersList filt = do
    users <- runDB $ selectList filt [Asc UserIdent]
    defaultLayout $ do
        setTitle . toHtml $ ("Member list" :: Text)
        $(widgetFile "members-list")
  where
    months = [1..12] :: [Int]

memberDetail :: User -> Widget
memberDetail u = do
    maybeLevel <- liftHandler $ case userLevel u of
            Nothing    -> return Nothing
            Just lvlId -> do (runDB $ get404 lvlId) >>= return . Just
    currency <- liftHandler $ getYesod >>= return . appCurrency . appSettings
    let kGrant = isJust $ userKeysGranted u
    let kReturn = isJust $ userKeysReturned u
    $(widgetFile "members-detail")

getMemberProfileR :: Handler Html
getMemberProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ ("Your profile" :: Text)
        let u = user
        $(widgetFile "profile")

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
            runDB $ replace memberId nu
            addMessage "success" "Profile updated"
            redirect $ if uid == memberId then MemberProfileR else MembersOverviewR

memberEditForm :: UserId -> Handler (Form User)
memberEditForm memberId = do
    (_uid, u) <- requireAuthPair
    master <- getYesod
    let currency = appCurrency $ appSettings master
    -- this kind of nesting is uneccessary, i just wanted to try to run both
    -- queries in single transaction
    (memberData, levels) <- runDB $ do
        m <- get404 memberId
        l <- selectList [LevelActive ==. True] [Asc LevelAmount]
        return (m, l)
    return $ memberEditForm' (userStaff u) levels currency memberData

-- XXX Maybe User -> Form User for new users (see example)
memberEditForm' :: Bool -> [Entity Level] -> Text -> User -> Form User
memberEditForm' isStaff levels currency u = renderBootstrap2 $ User
    <$> (if isStaff
            then areq textField "Nick" (Just $ userIdent u)
            else pure (userIdent u))
    <*> areq emailField "Email" (Just $ userEmail u)
    <*> pure (userPassword u) -- password
    <*> pure (userVerkey u) -- verkey
    <*> (if isStaff
            then areq checkBoxField "Verified" (Just $ userVerified u)
            else pure (userVerified u))
    <*> aopt textField "Full name" (Just $ userRealname u)
    <*> aopt textField "Alternative nick" (Just $ userAltnick u)
    <*> aopt phoneField "Phone number" (Just $ userPhone u)
    <*> (if isStaff
            then aopt levelField "Membership level" (Just $ userLevel u)
            else pure (userLevel u))
    <*> (if isStaff
            then aopt intField "Payments ID" (Just $ userPaymentsId u)
            else pure (userPaymentsId u)) -- paymentsId
    -- FIXME why the FUCK is dayField using month/day/year format ...
    <*> (if isStaff
            then dayToUTC <$> (areq dayField "Date joined" (Just $ utctDay $ userDateJoined u))
            else pure (userDateJoined u))
    <*> (if isStaff
            then (fmap dayToUTC) <$> (aopt dayField "Keys granted" (Just $ utctDay <$> userKeysGranted u))
            else pure (userKeysGranted u))
    <*> (if isStaff
            then (fmap dayToUTC) <$> (aopt dayField "Keys returned" (Just $ utctDay <$> userKeysReturned u))
            else pure (userKeysReturned u))
    <*> (if isStaff
            then areq (selectField optionsEnum) "State" (Just $ userState u)
            else pure (userState u))
    <*> (if isStaff
            then areq checkBoxField "Council" (Just $ userCouncil u)
            else pure (userCouncil u))
    <*> (if isStaff
            then areq checkBoxField "Staff" (Just $ userStaff u)
            else pure (userStaff u))
  where
    phoneField = strField 4 20 (\c -> isDigit c || c `elem` [' ', '+', '(', ')']) "Phone number"
    dayToUTC day = UTCTime day 0
    levelField = selectFieldList $ map (\(Entity lid lvl) -> (formatLevel lvl currency, lid)) levels

formatLevel :: Level -> Text -> Text
formatLevel lvl currency = levelName lvl <> " [" <> (showRational $ levelAmount lvl) <> " " <> currency <> "]"

{-
getMaybeLevel :: User -> Handler (Maybe Level)
getMaybeLevel user = case userLevel user of
            Nothing    -> return Nothing
            Just lvlId -> (runDB $ get404 lvlId) >>= return . Just
-}

editLink :: Route App -> Widget
editLink route = do
    (_, u) <- handlerToWidget $ requireAuthPair
    if userStaff u
        then [whamlet|[<a href=@{route}>edit</a>]|]
        else [whamlet||]
