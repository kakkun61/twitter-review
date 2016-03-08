module Handler.AccountAuth where

import Import
import Data.Text.Encoding       (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Text.Read
import Data.Maybe
import Web.Authenticate.OAuth   hiding (insert)

mkOAuth :: App -> OAuth
mkOAuth master = newOAuth { oauthServerName      = "twitter"
                          , oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
                          , oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
                          , oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize"
                          , oauthSignatureMethod = HMACSHA1
                          , oauthConsumerKey     = encodeUtf8 $ appTwitterOAuthKey $ appSettings master
                          , oauthConsumerSecret  = encodeUtf8 $ appTwitterOAuthSecret $ appSettings master
                          , oauthVersion         = OAuth10a
                          }

oauthTokenSecretName :: IsString a => a
oauthTokenSecretName = "oauth_token_secret"

oauthTokenName :: IsString a => a
oauthTokenName = "oauth_token"

oauthSessionName :: Text
oauthSessionName = "__oauth_token_secret"

getAccountAuthForwardR :: Handler Html
getAccountAuthForwardR = do
    setUltDestReferer
    master <- getYesod
    render <- getUrlRender
    let oauth' = (mkOAuth master) { oauthCallback = Just $ encodeUtf8 $ render AccountAuthCallbackR }
    tok <- getTemporaryCredential oauth' (authHttpManager master)
    setSession oauthSessionName $ bsToText $ fromMaybe "" $ lookup oauthTokenSecretName $ unCredential tok
    redirect $ authorizeUrl oauth' tok

getAccountAuthCallbackR :: Handler Html
getAccountAuthCallbackR = do
    master <- getYesod
    let oauth = mkOAuth master
    Just tokSec <- lookupSession oauthSessionName
    deleteSession oauthSessionName
    denied <- runInputGet $ iopt textField "denied"
    case denied of
        Just _ -> do
            setMessage "log-in failed"
            redirectUltDest $ HomeR
        Nothing -> do
            reqTok <-
                if oauthVersion oauth == OAuth10
                    then do
                        oaTok  <- runInputGet $ ireq textField "oauth_token"
                        return $ Credential [ (oauthTokenName, encodeUtf8 oaTok)
                                            , (oauthTokenSecretName, encodeUtf8 tokSec)
                                            ]
                    else do
                        (verifier, oaTok) <-
                            runInputGet $ (,) <$> ireq textField "oauth_verifier"
                                              <*> ireq textField "oauth_token"
                        return $ Credential [ ("oauth_verifier", encodeUtf8 verifier)
                                            , (oauthTokenName, encodeUtf8 oaTok)
                                            , (oauthTokenSecretName, encodeUtf8 tokSec)
                                            ]
            Credential bsDic <- getAccessToken oauth reqTok $ getHttpManager master
            let dic        = map (bsToText *** bsToText) bsDic
                tok        = fromJust $ lookup oauthTokenName dic
                userId     = read $ unpack $ fromJust $ lookup "user_id" dic
                screenName = fromJust $ lookup "screen_name" dic
            runDB $ do
                mAccount <- getBy $ UniqueAccount userId
                case mAccount of
                    Just (Entity uid (Account _ screenName' tok' tokSec')) -> do
                        when (screenName /= screenName' || tok /= tok' || tokSec /= tokSec') $
                            update uid [ AccountScreenName  =. screenName
                                       , AccountToken       =. tok
                                       , AccountTokenSecret =. tokSec
                                       ]
                    Nothing -> do
                        _ <- insert $ Account userId screenName tok tokSec
                        return ()
            redirect $ AccountSettingR screenName


bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
