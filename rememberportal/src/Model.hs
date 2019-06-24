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

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
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
    -- payments_id Int
    -- datejoined Day
    -- keysrcvd Day

    state MemberState
    council Bool
    staff Bool
    -- haskeys Bool

    UniqueUser ident
    UniqueEmail email
    deriving Typeable Show
|]
