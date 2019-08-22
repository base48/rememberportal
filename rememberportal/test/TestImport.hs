{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Enums                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)

-- Wiping the database
import Data.Maybe                           (fromJust)
import Data.Time.Format
import Database.Persist.Sqlite              (sqlDatabase, mkSqliteConnectionInfo, fkEnabled, createSqlitePoolFromInfo)
import Control.Monad.Logger                 (runLoggingT)
import Lens.Micro                           (set)
import Settings                             (appDatabaseConf, AppSettings(..))
import Yesod.Core                           (messageLoggerSource)
import Yesod.Auth.Email                     (loginR)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    let settingsWDummyAuth = settings { appAuthDummy = True }
    foundation <- makeFoundation settingsWDummyAuth
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to use a connection which has
    -- foreign key checks disabled.  Foreign key checks are enabled or disabled
    -- per connection, so this won't effect queries outside this function.
    --
    -- Aside: foreign key checks are enabled by persistent-sqlite, as of
    -- version 2.6.2, unless they are explicitly disabled in the
    -- SqliteConnectionInfo.

    let logFunc = messageLoggerSource app (appLogger app)

    let dbName = sqlDatabase $ appDatabaseConf $ appSettings app
        connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName

    pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        sqlBackend <- ask
        let queries = map (\t -> "DELETE FROM " ++ (connEscapeName sqlBackend $ DBName t)) tables
        forM_ queries (\q -> rawExecute q [])

getTables :: DB [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    get $ AuthR LoginR
    request $ do
        setMethod "POST"
        addToken
        addPostParam "email" $ userIdent u
        addPostParam "password" "IGNORED"
        setUrl $ AuthR loginR

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
createUser :: Text -> YesodExample App (Entity User)
createUser ident = runDB $ do
    user <- insertEntity User
        { userIdent = ident
        , userEmail = (ident <> "@example.org")
        , userPassword = Just "DOESNTMATTER"
        , userVerkey = Nothing
        , userVerified = True
        , userRealname = Nothing
        , userAltnick = Nothing
        , userPhone = Nothing
        , userLevel = Nothing
        , userPaymentsId = Nothing
        , userDateJoined = fromJust $ buildTime defaultTimeLocale [('Y', "2000")]
        , userKeysGranted = Nothing
        , userKeysReturned = Nothing
        , userState = Awaiting
        , userCouncil = False
        , userStaff = False
        }
    return user

acceptMember :: Entity User -> YesodExample App ()
acceptMember (Entity uid _) = runDB $ update uid [UserState =. Accepted]

createFixtures :: YesodExample App ()
createFixtures = do
    l1@(Entity lid1 _) <- runDB $ insertEntity Level { levelName = "Regular member",    levelAmount = 500,  levelActive = True, levelUserSelectable = True }
    l2@(Entity lid2 _) <- runDB $ insertEntity Level { levelName = "Regular member 1k", levelAmount = 1000, levelActive = True, levelUserSelectable = True }
    l3@(Entity lid3 _) <- runDB $ insertEntity Level { levelName = "Regular member 2k", levelAmount = 2000, levelActive = True, levelUserSelectable = True }
    l4@(Entity lid4 _) <- runDB $ insertEntity Level { levelName = "Student member",    levelAmount = 300,  levelActive = True, levelUserSelectable = False }

    u1@(Entity uid1 _) <- createUser "admin"
    u2@(Entity uid2 _) <- createUser "council"
    u3@(Entity uid3 _) <- createUser "member1"
    u4@(Entity uid4 _) <- createUser "member2"
    u5@(Entity uid5 _) <- createUser "awaiting"

    runDB $ do
        update uid1 [UserState =. Accepted, UserStaff =. True,   UserLevel =. Just lid3, UserKeysGranted =. Just month1]
        update uid2 [UserState =. Accepted, UserCouncil =. True, UserLevel =. Just lid1, UserKeysGranted =. Just month1]
        update uid3 [UserState =. Accepted, UserLevel =. Just lid4, UserKeysGranted =. Just month1]
        update uid4 [UserState =. Accepted, UserLevel =. Just lid1, UserKeysGranted =. Just month1]

    runDB $ do
        insertEntity Payment { paymentUser = Just uid3, paymentDate = month1, paymentAmount = 500, paymentKind = "fio", paymentKindId = "1234", paymentLocalAccount = "2900086515/2010", paymentRemoteAccount = "8", paymentIdentification = "", paymentJson = "", paymentStaffComment = "" }
        insertEntity Payment { paymentUser = Just uid3, paymentDate = month2, paymentAmount = 500, paymentKind = "fio", paymentKindId = "1235", paymentLocalAccount = "2900086515/2010", paymentRemoteAccount = "8", paymentIdentification = "", paymentJson = "", paymentStaffComment = "" }
        insertEntity Payment { paymentUser = Just uid3, paymentDate = month3, paymentAmount = 500, paymentKind = "fio", paymentKindId = "1236", paymentLocalAccount = "2900086515/2010", paymentRemoteAccount = "8", paymentIdentification = "", paymentJson = "", paymentStaffComment = "" }
        insertEntity Payment { paymentUser = Just uid3, paymentDate = month4, paymentAmount = 500, paymentKind = "fio", paymentKindId = "1237", paymentLocalAccount = "2900086515/2010", paymentRemoteAccount = "8", paymentIdentification = "", paymentJson = "", paymentStaffComment = "" }

    runDB $ do
        insertEntity Fee { feeUser = uid3, feeLevel = lid4, feePeriodStart = month1, feeAmount = 300 }
        insertEntity Fee { feeUser = uid3, feeLevel = lid4, feePeriodStart = month2, feeAmount = 300 }
        insertEntity Fee { feeUser = uid3, feeLevel = lid4, feePeriodStart = month3, feeAmount = 300 }
        insertEntity Fee { feeUser = uid3, feeLevel = lid4, feePeriodStart = month4, feeAmount = 300 }

    return ()
  where
    -- createLevel l = runDB $ insertEntity l
    --createPayment :: Payment -> YesodExample App ()
    --createPayment p = runDB $ (insertEntity p >> return ())
    month1 = fromJust $ buildTime defaultTimeLocale [('Y', "2000")]
    month2 = fromJust $ buildTime defaultTimeLocale [('Y', "2000"), ('m', "2")]
    month3 = fromJust $ buildTime defaultTimeLocale [('Y', "2000"), ('m', "3")]
    month4 = fromJust $ buildTime defaultTimeLocale [('Y', "2000"), ('m', "4")]
