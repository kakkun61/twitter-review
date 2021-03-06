module Foundation where

import Import.NoFoundation
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import qualified Yesod.Auth.GoogleEmail2 as GoogleEmail2
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Database.HDBC        (commit)
import Database.HDBC.MySQL  (Connection)
import Web.Authenticate.OAuth (OAuth (..), OAuthVersion (OAuth10a), newOAuth, SignMethod (HMACSHA1))
import qualified Model.Table.User as User
import qualified Slack
import qualified Data.Aeson.Types as Aeson

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appHttpManager :: Manager
    , appLogger      :: Logger
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
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- The defaultCsrfMiddleware:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addScript $ StaticR scripts_bootstrap_min_js
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ MasterLoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _)    _ = return Authorized
    isAuthorized (StaticR _)  _ = return Authorized
    isAuthorized FaviconR     _ = return Authorized
    isAuthorized RobotsR      _ = return Authorized
    isAuthorized HomeR        _ = return Authorized
    isAuthorized MasterLoginR _ = return Authorized
    isAuthorized _            _ = do
        mUserId <- maybeAuthId
        case mUserId of
            Just _ -> do
                return Authorized
            Nothing -> return AuthenticationRequired

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
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
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodRelational App where
    type YesodRelationalConnection App = Connection
    -- runRelational :: YesodRelationalMonad site a -> HandlerT site IO a
    runRelational action = do
        con <- liftIO connectDB
        runReaderT action con

instance YesodAuth App where
    type AuthId App = Int64

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runRelational $ do
        let ident = unpack $ credsIdent creds
        mToken <- lift GoogleEmail2.getUserAccessToken
        case mToken of
            Just token@(GoogleEmail2.Token accTok' tokTyp) -> do
                let accTok = unpack accTok'
                if (tokTyp /= bearer)
                then return $ ServerError $ "unexpected token type\n\texpected: " <> bearer <> "\n\tactual: " <> tokTyp
                else do
                    master <- lift getYesod
                    let manager = authHttpManager master
                    mDisplayName <- fmap (fmap unpack . join . fmap (GoogleEmail2.personDisplayName)) $ lift (GoogleEmail2.getPerson manager token)
                    mUser <- runQuery (relationalQuery (userFromEmail ident)) ()
                    case mUser of
                        [User uid _ _ mDisplayName' token'] -> do
                            when (mDisplayName /= mDisplayName' || accTok /= token') $ do
                                void $ runUpdate (updateUser uid mDisplayName accTok) ()
                                run commit
                            return $ Authenticated uid
                        [] -> do
                            _ <- runInsert User.insertUserNoId (UserNoId ident (takeWhile (/= '@') ident) mDisplayName accTok)
                            run commit
                            uids <- runQuery selectLastInsertId ()
                            case uids of
                                [uid] -> do
                                    return $ Authenticated uid
                                _ -> return $ ServerError "unexpected"
                        _ -> return $ ServerError "unexpected"
            Nothing ->
                return $ ServerError "no token gotten"
        where
            bearer :: Text
            bearer = "Bearer"
            userFromEmail :: String -> Relation () User
            userFromEmail email = relation $ do
                u <- query User.user
                wheres $ u ! User.email' .=. value email
                return u
            updateUser :: Int64 -> Maybe String -> String -> Update ()
            updateUser uid displayName token =
                typedUpdate User.tableOfUser . updateTarget $ \proj -> do
                    User.displayName' <-# value displayName
                    User.token' <-# value token
                    wheres $ proj ! User.id' .=. value uid

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins m = [GoogleEmail2.authGoogleEmailSaveToken
                         (appGoogleOAuthKey $ appSettings m)
                         (appGoogleOAuthSecret $ appSettings m)]

    authHttpManager = getHttpManager

    loginHandler = redirect GoogleEmail2.forwardUrl

    maybeAuthId = defaultMaybeAuthId

instance YesodAuthRelational App where
    type AuthModel App = User

    getAuthModel ident = do
        us <- runRelational $ runQuery User.selectUser ident
        case us of
            [] -> return Nothing
            u : _ -> return $ Just u

mkTwitterOAuth :: App -> OAuth
mkTwitterOAuth master = newOAuth { oauthServerName      = "twitter"
                                 , oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
                                 , oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
                                 , oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize"
                                 , oauthSignatureMethod = HMACSHA1
                                 , oauthConsumerKey     = encodeUtf8 $ appTwitterOAuthKey $ appSettings master
                                 , oauthConsumerSecret  = encodeUtf8 $ appTwitterOAuthSecret $ appSettings master
                                 , oauthVersion         = OAuth10a
                                 }

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

postSlack :: Aeson.Value -> Handler ()
postSlack body = do
    master <- getYesod
    let uri = appSlackIncomingWebhook $ appSettings master
    liftIO $ Slack.post uri body

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
