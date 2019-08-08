{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Foundation where

import Import.NoFoundation
import Registration

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)
import Data.Char            (isAlphaNum, isAscii, isDigit)
import Data.Maybe           (fromJust)

import Yesod.Auth.Email
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Auth.Message as Msg
import qualified Yesod.Auth.Util.PasswordStore as PS
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.Text.Encoding as TE
import           Data.Text.Encoding.Error (lenientDecode)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic
/auth   AuthR   Auth   getAuth

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ HomeR GET

/member/profile        MemberProfileR   GET      !login
/member/edit/#UserId   MemberEditR      GET POST !staff

/members               MembersOverviewR GET      !member
/members/list/accepted MembersAcceptedR GET      !member
/members/list/rejected MembersRejectedR GET      !member
/members/list/awaiting MembersAwaitingR GET      !member
/members/list/ex       MembersExR       GET      !member
/members/list/level/#LevelId MembersLevelR GET   !member
/members/list/nolevel  MembersNoLevelR  GET      !member

/payments                       PaymentsR          GET !member

/admin                          AdminR             GET      !staff
/admin/levels/new               LevelNewR          POST     !staff
/admin/levels/edit/#LevelId     LevelEditR         GET POST !staff
|]

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- FIXME need to have approot set because of the email auth
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case fromMaybe "" $ appRoot $ appSettings app of
            "" -> getApprootText guessApproot app req
            root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = sslOnlySessions $
            fmap Just $ defaultClientSessionBackend sessionTimeoutMinutes keyFile
      where
        keyFile = "client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware =
        sslOnlyMiddleware sessionTimeoutMinutes .
        defaultCsrfMiddleware .
        defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        muser <- maybeAuthPair
        msgs <- getMessages

        let (isLogged, isMember, isStaff) = case muser of
                Nothing -> (False, False, False)
                Just (_uid,  u) -> (True
                                   , userState u == Accepted || userStaff u == True
                                   , userStaff u == True
                                   )

        -- Define the menu items of the header.
        let menuItems =
                [ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isLogged
                    }
                , MenuItem
                    { menuItemLabel = "View profile"
                    , menuItemRoute = MemberProfileR
                    , menuItemAccessCallback = isLogged
                    }
                , MenuItem
                    { menuItemLabel = "Edit profile"
                    , menuItemRoute = MemberEditR $ fst $ fromJust muser
                    , menuItemAccessCallback = isLogged
                    }
                , MenuItem
                    { menuItemLabel = "Change password"
                    , menuItemRoute = AuthR setpassR
                    , menuItemAccessCallback = isLogged
                    }
                , MenuItem
                    { menuItemLabel = "Administration"
                    , menuItemRoute = AdminR
                    , menuItemAccessCallback = isStaff
                    }
                , MenuItem
                    { menuItemLabel = "Members overview"
                    , menuItemRoute = MembersOverviewR
                    , menuItemAccessCallback = isMember
                    }
                , MenuItem
                    { menuItemLabel = "Members list"
                    , menuItemRoute = MembersAcceptedR
                    , menuItemAccessCallback = isMember
                    }
                , MenuItem
                    { menuItemLabel = "Finance"
                    , menuItemRoute = PaymentsR
                    , menuItemAccessCallback = isMember
                    }
                , MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = not isLogged
                    }
                , MenuItem
                    { menuItemLabel = "Reset password"
                    , menuItemRoute = AuthR forgotPasswordR
                    , menuItemAccessCallback = not isLogged
                    }
                , MenuItem
                    { menuItemLabel = "Registration"
                    , menuItemRoute = AuthR registerR
                    , menuItemAccessCallback = not isLogged
                    }
                ]

        let navbarItems = [x | x <- menuItems, menuItemAccessCallback x]
        let defAlert x = if x == "" then "primary" else x

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_min_css
            addStylesheet $ StaticR css_bootstrap_responsive_min_css
            addStylesheet $ StaticR css_font_awesome_css
            addStylesheet $ StaticR css_main_css
            addScript $ StaticR js_jquery_1_7_2_min_js
            addScript $ StaticR js_bootstrap_min_js
            let orgName = appOrgName $ appSettings master
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    -- FIXME
    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    isAuthorized (AuthR _) _              = return Authorized
    isAuthorized (StaticR _) _            = return Authorized
    isAuthorized HomeR _                  = return Authorized
    isAuthorized FaviconR _               = return Authorized
    isAuthorized RobotsR _                = return Authorized
    isAuthorized (MemberEditR memberId) _ = do
        muser <- maybeAuthPair
        case muser of
            Nothing -> return AuthenticationRequired
            Just (uid, u) | userStaff u     -> return Authorized
                          | memberId == uid -> return Authorized
                          | otherwise       -> return $ Unauthorized "Admin access only"
    isAuthorized route _writable
        | "staff" `member` routeAttrs route = do
            muser <- maybeAuthPair
            case muser of
                Nothing -> return AuthenticationRequired
                Just (_, u)
                    | userStaff u -> return Authorized
                    | otherwise   -> return $ Unauthorized "Admin access only"
        | "member" `member` routeAttrs route = do
            muser <- maybeAuthPair
            case muser of
                Nothing -> return AuthenticationRequired
                Just (_, u)
                    | userStaff u             -> return Authorized
                    | userState u == Accepted -> return Authorized
                    | otherwise               -> return $ Unauthorized "Member access only"
        | "login" `member` routeAttrs route = do
            muid <- maybeAuthId
            case muid of
                Nothing -> return AuthenticationRequired
                Just _  -> return Authorized
        | otherwise = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = False

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = if credsPlugin creds `elem` ["email", "email-verify", "username"]
        then liftHandler $ runDB $ do
            -- $logDebug $ "authenticate " <> (pack $ show creds)
            x <- getBy $ UniqueEmail $ credsIdent creds
            case x of
                Just (Entity uid _) -> return $ Authenticated uid
                Nothing -> return $ ServerError $ credsPlugin creds
        else error (show creds)

    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authEmail]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- FIXME what is this
{-
-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager
-}

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

strField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Int -> Int -> (Char -> Bool) -> Text -> Field m Text
strField minLen maxLen charPred fieldName = check f textField
  where
    f ident | length ident < minLen    = Left (fieldName <> ": too short" :: Text)
            | length ident > maxLen    = Left (fieldName <> ": too long" :: Text)
            | not $ all charPred ident = Left (fieldName <> ": invalid character" :: Text)
            | otherwise                = Right ident


instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = MemberProfileR

    addUnverified email verkey = do
        toParent <- getRouteToParent
        liftHandler $ do
            ((result, _widget), _enctype) <- runFormPost memberNewForm
            case result of
                FormMissing -> do
                    addMessage "danger" $ toHtml ("Form missing" :: Text)
                    redirect $ toParent registerR
                FormFailure fs -> do
                    forM_ fs (addMessage "danger" . toHtml)
                    redirect $ toParent registerR -- FIXME render directly?
                    -- registerHandler -- somehow
                FormSuccess nu -> do
                    let withKey = nu { userVerkey = Just verkey, userEmail = email }
                    runDB $ insert withKey

    sendVerifyEmail email _verkey verurl = do
        $logInfo $ "Verification URL for " <> email <> ": " <> verurl
        master <- getYesod
        liftIO $ sendRegEmail (appSettings master) email verurl

    getVerifyKey =
        liftHandler . runDB . fmap (join . fmap userVerkey) . get

    setVerifyKey uid key = liftHandler $ runDB $ update uid [UserVerkey =. Just key]

    -- FIXME Warning! If you have persisted the AuthEmailId site somewhere, this method should delete that key, or make it unusable in some fashion. Otherwise, the same key can be used multiple times!
    verifyAccount uid = liftHandler $ runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _u -> do
                update uid [UserVerified =. True, UserVerkey =. Nothing]
                return $ Just uid

    getPassword = liftHandler . runDB . fmap (join . fmap userPassword) . get

    setPassword uid pass = liftHandler . runDB $ update uid [UserPassword =. Just pass]

    getEmailCreds intext = liftHandler $ runDB $ do
        mu <- getBy $ UniqueEmail intext
        case mu of
            Nothing -> do
                mv <- getBy $ UniqueUser intext
                case mv of
                    Nothing -> return Nothing
                    Just (Entity uid u) -> f uid u
            Just (Entity uid u) -> f uid u
      where
        f uid row = return $ Just $ EmailCreds
            { emailCredsId = uid
            , emailCredsAuthId = Just uid
            , emailCredsStatus = userVerified row && isJust (userPassword row)
            , emailCredsVerkey = userVerkey row
            , emailCredsEmail = userEmail row
            }


    getEmail = liftHandler . runDB . fmap (fmap userEmail) . get

    -- pbkdf2
    hashAndSaltPassword plain = liftIO $ do
        let plain8 = TE.encodeUtf8 plain
        let expn = 16
        pass <- PS.makePasswordWith PS.pbkdf2 plain8 expn
        return $ TE.decodeUtf8With lenientDecode pass

    verifyPassword plain salted = do
        master <- getYesod
        if appAuthDummy $ appSettings master
            then return True
            else return $ PS.verifyPasswordWith PS.pbkdf2 (2^) plain8 salted8
      where
        plain8 = TE.encodeUtf8 plain
        salted8 = TE.encodeUtf8 salted

    registerHandler = do
        toParent <- getRouteToParent
        liftHandler $ do
            (widget, enctype) <- generateFormPost memberNewForm
            authLayout $ do
                setTitleI Msg.RegisterLong
                [whamlet|
                    <form .form-horizontal method=post action=@{toParent registerR} enctype=#{enctype}>
                        <div class="alert">
                            <strong>Warning!</strong> Details you provide here are visible for other members. Do not add any personal details you want to keep private! Email is required.
                        ^{widget}
                        <div .form-actions>
                            <button type=submit .btn .btn-primary>_{Msg.Register}
                |]

    emailLoginHandler toParent = do
        (widget, enctype) <- generateFormPost loginForm
        [whamlet|
            <form .form-horizontal method=post action=@{toParent loginR} enctype=#{enctype}>
                ^{widget}
                <div .form-actions>
                    <button type=submit .btn .btn-primary>Login
        |]
      where
        loginForm = renderBootstrap2 $ (,)
            <$> areq textField (fs "Username" "email") Nothing
            <*> areq passwordField (fs "Password" "password") Nothing
        fs msg name = FieldSettings {
            fsLabel = msg,
            fsTooltip = Nothing,
            fsId = Just name,
            fsName = Just name,
            fsAttrs = []
        }

    setPasswordHandler needOld = selectRep $ provideRep $ do
        toParent <- getRouteToParent
        liftHandler $ do
            (widget, enctype) <- generateFormPost changeForm
            authLayout $ do
                setTitleI Msg.SetPassTitle
                [whamlet|
                    <h3>_{Msg.SetPass}
                    <form .form-horizontal method=post action=@{toParent setpassR} enctype=#{enctype}>
                        ^{widget}
                        <div .form-actions>
                            <button type=submit .btn .btn-primary>_{Msg.SetPassTitle}
                |]
      where
        changeForm = renderBootstrap2 $ (,,)
            <$> (if needOld
                    then areq passwordField currentPasswordSettings Nothing
                    else pure "")
            <*> areq passwordField newPasswordSettings Nothing
            <*> areq passwordField confirmPasswordSettings Nothing
        currentPasswordSettings =
             FieldSettings {
                 fsLabel = SomeMessage Msg.CurrentPassword,
                 fsTooltip = Nothing,
                 fsId = Just "currentPassword",
                 fsName = Just "current",
                 fsAttrs = []
             }
        newPasswordSettings =
            FieldSettings {
                fsLabel = SomeMessage Msg.NewPass,
                fsTooltip = Nothing,
                fsId = Just "newPassword",
                fsName = Just "new",
                fsAttrs = []
            }
        confirmPasswordSettings =
            FieldSettings {
                fsLabel = SomeMessage Msg.ConfirmPass,
                fsTooltip = Nothing,
                fsId = Just "confirmPassword",
                fsName = Just "confirm",
                fsAttrs = []
            }

    forgotPasswordHandler = do
        (widget, enctype) <- generateFormPost forgotPasswordForm
        toParent <- getRouteToParent
        authLayout $ do
            setTitleI Msg.PasswordResetTitle
            [whamlet|
                <p>_{Msg.PasswordResetPrompt}
                <form .form-horizontal method=post action=@{toParent forgotPasswordR} enctype=#{enctype}>
                    ^{widget}
                    <div .form-actions>
                        <button type=submit .btn .btn-primary>_{Msg.SendPasswordResetEmail}
            |]
      where
        forgotPasswordForm = renderBootstrap2 $ (,)
            <$> areq textField emailSettings Nothing
            <*> pure Nothing
        emailSettings =
            FieldSettings {
                fsLabel = SomeMessage Msg.ProvideIdentifier,
                fsTooltip = Nothing,
                fsId = Just "forgotPassword",
                fsName = Just "email",
                fsAttrs = [("autofocus", "")]
            }

sessionTimeoutMinutes :: Int
sessionTimeoutMinutes = 120

memberNewForm :: Form User
memberNewForm = renderBootstrap2 $ User
    <$> areq identField "Username" Nothing
    <*> areq emailField (fs "Email" "email") Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure False
    <*> aopt textField "Full name" Nothing
    <*> aopt textField "Alternative nick" Nothing
    <*> aopt phoneField "Phone number" Nothing
    <*> pure Nothing
    <*> pure Nothing -- FIXME allocate payments id
    <*> lift (liftIO getCurrentTime)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Awaiting
    <*> pure False
    <*> pure False
  where
    fs msg name = FieldSettings {
        fsLabel = msg,
        fsTooltip = Nothing,
        fsId = Just name,
        fsName = Just name,
        fsAttrs = []
    }
    phoneField = strField 4 20 (\c -> isDigit c || c `elem` [' ', '+', '(', ')']) "Phone number"
    identField = strField 1 30 (\c -> (isAlphaNum c && isAscii c) || c == '.' || c == '-' || c == '_') "Username"

-- FIXME move this somewhere else
showRational :: Rational -> Text
showRational x = pack $ show $ fromRational x
