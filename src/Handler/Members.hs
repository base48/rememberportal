{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Members where

import Import

import Data.Maybe (fromJust) -- eww
import Handler.Payments

import qualified Data.Map.Strict as Map

getMembersOverviewR :: Handler Html
getMembersOverviewR = do
    num_total <- countMembers [UserState ==. Accepted]
    num_awaiting <- countMembers [UserState ==. Awaiting]
    num_ex <- countMembers [UserState ==. Exmember]
    num_rejected <- countMembers [UserState ==. Rejected]
    num_suspended <- countMembers [UserState ==. Suspended]
    d <- runDB $ debtors
    monthly <- getMonthly
    defaultLayout $ do
        setTitle . toHtml $ ("Members overview" :: Text)
        $(widgetFile "members-overview")

getMonthly :: Handler Rational
getMonthly = do
    isFlexible <- getYesod >>= return . appFlexibleFees . appSettings
    runDB $ do
        users <- selectList [UserState ==. Accepted, UserLevel !=. Nothing] []
        levels <- selectList [LevelActive ==. True] []
        let levelMap = Map.fromList $ map (\(Entity lid l) -> (lid, l)) levels
        let f (Entity _ u) = fst $ actualFee isFlexible u (Map.findWithDefault defaultLevel (fromJust $ userLevel u) levelMap)
        return $ sum $ map f users
  where
    defaultLevel = Level { levelAmount = 0, levelName = "", levelActive = True }

getMembersAcceptedR :: Handler Html
getMembersAcceptedR = membersList [UserState ==. Accepted]

getMembersRejectedR :: Handler Html
getMembersRejectedR = membersList [UserState ==. Rejected]

getMembersAwaitingR :: Handler Html
getMembersAwaitingR = membersList [UserState ==. Awaiting]

getMembersExR :: Handler Html
getMembersExR = membersList [UserState ==. Exmember]

getMembersSuspendedR :: Handler Html
getMembersSuspendedR = membersList [UserState ==. Suspended]

getMembersLevelR :: LevelId -> Handler Html
getMembersLevelR lid = membersList [UserLevel ==. Just lid, UserState ==. Accepted]

getMembersNoLevelR :: Handler Html
getMembersNoLevelR = membersList [UserState ==. Accepted, UserLevel ==. Nothing]

getMembersKeysR :: Handler Html
getMembersKeysR = membersList [UserKeysGranted !=. Nothing, UserKeysReturned ==. Nothing]

getMembersNoKeysR :: Handler Html
getMembersNoKeysR = membersList [UserState ==. Accepted, UserKeysGranted ==. Nothing]

membersList :: [Filter User] -> Handler Html
membersList filt = do
    users <- runDB $ selectList filt [Asc UserIdent]
    defaultLayout $ do
        setTitle . toHtml $ ("Member list" :: Text)
        $(widgetFile "members-list")

countMembers :: [Filter User] -> Handler Int
countMembers filt = runDB $ count filt

memberDetail :: UserId -> User -> Widget
memberDetail uid u = do
    maybeLevel <- liftHandler $ case userLevel u of
            Nothing    -> return Nothing
            Just lvlId -> do (runDB $ get404 lvlId) >>= return . Just

    settings <- liftHandler $ getYesod >>= return . appSettings
    let currency = appCurrency settings
    let kGrant = isJust $ userKeysGranted u
    let kReturn = isJust $ userKeysReturned u
    let flexibleFees = appFlexibleFees settings
    $(widgetFile "members-detail")
  where
    veryInefficientBalance = do -- ewwww
        b <- liftHandler $ runDB $ memberBalance uid
        balanceWidget b

getMemberProfileR :: Handler Html
getMemberProfileR = do
    (uid, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ ("Your profile" :: Text)
        let u = user
        $(widgetFile "profile")

editLink :: Route App -> Widget
editLink = staffLink "edit"

staffLink :: Text -> Route App -> Widget
staffLink text route = do
    (_, u) <- handlerToWidget $ requireAuthPair
    if userStaff u
        then [whamlet|[<a href=@{route}>#{text}</a>]|]
        else [whamlet||]
