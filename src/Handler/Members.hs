{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Members where

import Import

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
