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
    num_paying = 1337 :: Int
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

getProfileEditR :: Handler Html
getProfileEditR = do
    (_, user) <- requireAuthPair
    (widget, enctype) <- generateFormPost $ memberEditForm user
    profileEditHelper widget enctype

postProfileEditR :: Handler Html
postProfileEditR = do
    (uid, user) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ memberEditForm user
    $logDebug (pack $ show result)
    case result of
        FormMissing -> do
            addMessage "danger" $ toHtml ("Form missing" :: Text)
            redirect ProfileEditR
        FormFailure fs -> do
            forM_ fs (addMessage "danger" . toHtml)
            profileEditHelper widget enctype
        FormSuccess nu -> do
            runDB $ replace uid nu
            addMessage "success" "Profile updated"
            redirect ProfileR

profileEditHelper :: Widget -> Enctype -> Handler Html
profileEditHelper widget enctype = defaultLayout $ do
        setTitle . toHtml $ ("Edit profile" :: Text)
        $(widgetFile "profile-edit")

-- XXX Maybe User -> Form User for new users (see example)
memberEditForm :: User -> Form User
memberEditForm u = renderBootstrap2 $ User
    <$> pure (userIdent u) -- ident
    <*> areq emailField "Email" (Just $ userEmail u)
    <*> pure (userPassword u) -- password
    <*> pure (userVerkey u) -- verkey
    <*> pure (userVerified u) --verified
    <*> aopt textField "Full name" (Just (userRealname u)) -- FIXME req
    <*> aopt textField "Alternative nick" (Just (userAltnick u))
    <*> aopt phoneField "Phone number" (Just (userPhone u))
    <*> pure (userState u)
    <*> pure (userCouncil u)
    <*> pure (userStaff u)
  where
    phoneField = strField 4 20 (\c -> isDigit c || c `elem` [' ', '+', '(', ')']) "Phone number"
