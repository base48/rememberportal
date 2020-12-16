{-# LANGUAGE TemplateHaskell #-}
module Enums where

import Database.Persist.TH

data MemberState = Awaiting | Accepted | Rejected | Exmember | Suspended
    deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "MemberState"
