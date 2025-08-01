{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances #-}
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
    altcontact Text Maybe

    level LevelId Maybe
    levelActualAmount Rational default=0 -- used for flexible fees
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
    kindId Text -- unique id for given kind
    localAccount Text
    remoteAccount Text
    identification Text

    -- type Text is used in order to be compatible with https://www.sqlite.org/json1.html
    -- even though we aren't using it
    json Text

    staffComment Text

    UniqueKindId kind kindId
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
    deriving Typeable Show
|]
