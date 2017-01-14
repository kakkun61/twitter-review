module Handler.AccountAuth
    ( getAccountAuthForwardR
    , getAccountAuthCallbackR
    ) where

import Import
import Data.Text.Encoding       (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Text.Read
import Data.Maybe
import Web.Authenticate.OAuth   hiding (insert)
import Database.HDBC (IConnection, commit)
import Model.Table.Account (Account (..))
import qualified Model.Table.Account as Account
import Model.Table.UserAccountRelation (UserAccountRelationNoId (..))
import qualified Model.Table.UserAccountRelation as UserAccountRelation

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
    uid <- requireAuthId
    let oauth = mkOAuth master
    Just tokSec <- lookupSession oauthSessionName
    deleteSession oauthSessionName
    denied <- runInputGet $ iopt textField "denied"
    case denied of
        Just _ -> do
            setMessage "log-in failed"
            redirectUltDest HomeR
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
                tok        = unpack $ fromJust $ lookup oauthTokenName dic
                userId     = read $ unpack $ fromJust $ lookup "user_id" dic
                screenName = unpack $ fromJust $ lookup "screen_name" dic
            runRelational $ store uid userId screenName tok $ unpack tokSec
            clearUltDest
            redirect $ AccountSettingR userId
    where
        store :: (IConnection (YesodRelationalConnection site)) => AuthId App -> Int64 -> String -> String -> String -> YesodRelationalMonad site ()
        store uid userId screenName token tokenSecret = do
            mAccount <- flip runQuery () $ relationalQuery $ relation $ do
                            u <- query Account.account
                            wheres $ u ! Account.id' .=. value userId
                            return u
            case mAccount of
                [Account _ screenName' tok' tokSec']
                    | screenName /= screenName' || token /= tok' || tokenSecret /= tokSec' -> do
                        void $ runUpdate (updateAccount userId screenName token tokenSecret) ()
                        run commit
                    | otherwise -> return ()
                [] -> do
                    void $ runInsert Account.insertAccount $ Account userId screenName token tokenSecret
                    void $ runInsert UserAccountRelation.insertUserAccountRelationNoId $ UserAccountRelationNoId uid userId $ convert Admin
                    run commit
                _ -> error "unexpected"
        updateAccount :: Int64 -> String -> String -> String -> Update ()
        updateAccount aid screenName token tokenSecret =
            typedUpdate Account.tableOfAccount . updateTarget $ \proj -> do
                Account.screenName' <-# value screenName
                Account.token' <-# value token
                Account.tokenSecret' <-# value tokenSecret
                wheres $ proj ! Account.id' .=. value aid

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
