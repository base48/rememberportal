{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Enums

-- You can find more information on persistent and how to declare entities
-- at: http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    ident Text
    email Text

    password Text Maybe
    verkey Text Maybe -- used for resetting passwords
    verified Bool

    realname Text Maybe
    altnick Text Maybe
    phone Text Maybe

    level LevelId Maybe
    paymentsId Int Maybe

    dateJoined UTCTime
    keysGranted UTCTime Maybe
    keysReturned UTCTime Maybe

    state MemberState
    council Bool
    staff Bool

    UniqueUser ident
    UniqueEmail email
    UniquePaymentsId paymentsId !force -- force acknowledges that two NULLs are considered unique
    deriving Typeable Show

Payment
    user UserId Maybe
    date UTCTime
    amount Rational
    kind Text -- fio, btc
    localAccount Text
    remoteAccount Text
    identification Text

    json ByteString -- full payment data
--
--  paymentType Text
--  csym Text Maybe
--  vsym Text Maybe
--  ssym Text Maybe
--  identification Text
--  message Text
--  parsed Text
--  correctionRequired Bool

    deriving Typeable Show

Fee
    user UserId
    level LevelId
    periodStart UTCTime
    amount Rational
    deriving Typeable Show

Level
    name Text
    amount Rational
    active Bool
    userSelectable Bool
    deriving Typeable Show
|]
