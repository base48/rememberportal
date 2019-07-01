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
getMembersAcceptedR = membersList Accepted

getMembersRejectedR :: Handler Html
getMembersRejectedR = membersList Rejected

getMembersAwaitingR :: Handler Html
getMembersAwaitingR = membersList Awaiting

getMembersExR :: Handler Html
getMembersExR = membersList Exmember

membersList :: MemberState -> Handler Html
membersList ms = do
    users <- runDB $ selectList [UserState ==. ms] [Asc UserIdent]
    defaultLayout $ do
        setTitle . toHtml $ ("Member list" :: Text)
        $(widgetFile "members-list")
  where
    months = [1..12] :: [Int]

memberDetail :: User -> Widget
memberDetail u = $(widgetFile "members-detail")

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ ("Your profile" :: Text)
        let u = user
        $(widgetFile "profile")

profileEditHelper :: UserId -> Widget -> Enctype -> Handler Html
profileEditHelper memberId widget enctype = defaultLayout $ do
        setTitle . toHtml $ ("Edit profile" :: Text)
        $(widgetFile "profile-edit")

getProfileEditMemberR :: UserId -> Handler Html
getProfileEditMemberR memberId = do
    (_uid, u) <- requireAuthPair
    memberData <- runDB $ get404 memberId
    (widget, enctype) <- generateFormPost $ memberEditForm (userStaff u) memberData
    profileEditHelper memberId widget enctype

postProfileEditMemberR :: UserId -> Handler Html
postProfileEditMemberR memberId = do
    (uid, u) <- requireAuthPair
    memberData <- runDB $ get404 memberId
    ((result, widget), enctype) <- runFormPost $ memberEditForm (userStaff u) memberData
    -- $logDebug (pack $ show result)
    case result of
        FormMissing -> do
            addMessage "danger" $ toHtml ("Form missing" :: Text)
            redirect $ ProfileEditMemberR memberId
        FormFailure fs -> do
            forM_ fs (addMessage "danger" . toHtml)
            profileEditHelper memberId widget enctype
        FormSuccess nu -> do
            runDB $ replace memberId nu
            addMessage "success" "Profile updated"
            redirect $ if uid == memberId then ProfileR else MembersOverviewR

-- XXX Maybe User -> Form User for new users (see example)
memberEditForm :: Bool -> User -> Form User
memberEditForm isStaff u = renderBootstrap2 $ User
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

editLink :: Route App -> Widget
editLink route = do
    (_, u) <- handlerToWidget $ requireAuthPair
    if userStaff u
        then [whamlet|[<a href=@{route}>edit</a>]|]
        else [whamlet||]
