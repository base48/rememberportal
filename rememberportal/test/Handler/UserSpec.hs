{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.UserSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Profile page" $ do
        it "asserts no access to profile for anonymous users" $ do
            get MemberProfileR
            statusIs 303
            route <- getLocation
            assertEq "redirect to login page" route (Right (AuthR LoginR))

        it "asserts access to profile for authenticated users" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get MemberProfileR
            statusIs 200

    describe "Homepage" $ do
        it "redirects to login page for unauthenticated users" $ do
            get HomeR
            statusIs 303
            route <- getLocation
            assertEq "redirect to login page" route (Right (AuthR LoginR))
            _ <- followRedirect
            statusIs 200

        -- This is a simple example of using a database access in a test.  The
        -- test will succeed for a fresh scaffolded site with an empty database,
        -- but will fail on an existing database with a non-empty user table.
        it "leaves the user table empty" $ do
            get HomeR
            statusIs 303
            users <- runDB $ selectList ([] :: [Filter User]) []
            assertEq "user table empty" 0 $ length users

        it "redirects to profile when user is authenticated" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity
            get HomeR
            statusIs 303
            route <- getLocation
            assertEq "redirect to login page" route (Right (MemberProfileR))
            _ <- followRedirect
            statusIs 200

    describe "Members overview" $ do
        it "is not accessible to unauthenticated users" $ do
            get MembersOverviewR
            statusIs 303
            route <- getLocation
            assertEq "redirect to login page" route (Right (AuthR LoginR))

        it "is not accessible to awaiting users" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get MembersOverviewR
            statusIs 403

        it "is available to accepted members" $ do
            userEntity <- createUser "foo"
            acceptMember userEntity
            authenticateAs userEntity

            get MembersOverviewR
            statusIs 200

    describe "Members edit" $ do
        it "cannot be used by unauthenticaed users" $ do
            (Entity uid _) <- createUser "foo"
            request $ do
                setMethod "POST"
                setUrl $ MemberEditR uid
            statusIs 403

        it "can be used by ordinary user to edit their own profile" $ do
            userEntity <- createUser "foo"
            let (Entity uid _) = userEntity
            authenticateAs userEntity

            get $ MemberEditR uid
            statusIs 200
            request $ do
                setMethod "POST"
                addToken
                byLabelExact "Email" "foo@example.com"
                byLabelExact "Phone number" "%2B420606000666"
                setUrl $ MemberEditR uid
            statusIs 303

            mnu <- runDB $ getBy $ UniqueUser "foo"
            let (Just (Entity _ nu)) = mnu
            assertEq "phone number was updated" (Just "+420606000666") (userPhone nu)

        it "cannot be used by ordinary user to edit others' profile" $ do
            userEntity <- createUser "foo"
            otherEntity <- createUser "bar"
            let (Entity uid _) = userEntity
            let (Entity ouid _) = otherEntity
            acceptMember userEntity
            authenticateAs userEntity

            get $ MemberEditR uid
            statusIs 200
            request $ do
                setMethod "POST"
                addToken
                byLabelExact "Email" "somethingelse@example.com"
                byLabelExact "Phone number" "+420606000666"
                setUrl $ MemberEditR ouid
            statusIs 403

            mnu <- runDB $ getBy $ UniqueUser "foo"
            let (Just (Entity _ nu)) = mnu
            assertEq "phone number was not updated" Nothing (userPhone nu)

        it "can be used by staff to edit others' profile" $ do
            userEntity <- createUser "admin"
            otherEntity <- createUser "bar"
            let (Entity uid _) = userEntity
            let (Entity ouid _) = otherEntity
            runDB $ update uid [ UserState =. Accepted, UserStaff =. True ]
            authenticateAs userEntity

            get $ MemberEditR ouid
            statusIs 200
            request $ do
                setMethod "POST"
                addToken
                byLabelExact "Nick" "bar"
                byLabelExact "State" "1"
                byLabelExact "Email" "bar@example.com"
                byLabelExact "Phone number" "%2B420606000666"
                byLabelExact "Date joined" "2019-07-05"
                setUrl $ MemberEditR ouid
            printBody
            statusIs 303

            mnu <- runDB $ getBy $ UniqueUser "bar"
            let (Just (Entity _ nu)) = mnu
            assertEq "phone number was updated" (Just "+420606000666") (userPhone nu)

        it "creates fixtures" $ do
            createFixtures
