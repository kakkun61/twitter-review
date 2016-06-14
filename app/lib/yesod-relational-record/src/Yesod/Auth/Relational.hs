module Yesod.Auth.Relational where

-- | Retrieves user credentials from the session, if user is authenticated.
--
-- This function does /not/ confirm that the credentials are valid, see
-- 'maybeAuthIdRaw' for more information.
--
-- Since 1.1.2
defaultMaybeAuthId
    :: (YesodAuthPersist master, Typeable (AuthModel master))
    => HandlerT master IO (Maybe (AuthId master))
defaultMaybeAuthId = runMaybeT $ do
    s   <- MaybeT $ lookupSession credsKey
    aid <- MaybeT $ return $ fromPathPiece s
    _   <- MaybeT $ cachedAuth aid
    return aid

cachedAuth
    :: (YesodAuthPersist master, Typeable (AuthModel master))
    => AuthId master -> HandlerT master IO (Maybe (AuthModel master))
cachedAuth
    = fmap unCachedMaybeAuth
    . cached
    . fmap CachedMaybeAuth
    . getAuthModel

newtype CachedMaybeAuth val = CachedMaybeAuth { unCachedMaybeAuth :: Maybe val }
    deriving Typeable

-- | Similar to 'maybeAuthId', but additionally look up the value associated
-- with the user\'s database identifier to get the value in the database. This
-- assumes that you are using a Persistent database.
--
-- Since 1.1.0
maybeAuth :: ( YesodAuthPersist master
             , val ~ AuthModel master
             , Key val ~ AuthId master
             , PersistEntity val
             , Typeable val
             ) => HandlerT master IO (Maybe (Entity val))
maybeAuth = runMaybeT $ do
    (aid, ae) <- MaybeT maybeAuthPair
    return $ Entity aid ae

-- | Similar to 'maybeAuth', but doesn’t assume that you are using a
-- Persistent database.
--
-- Since 1.4.0
maybeAuthPair :: (YesodAuthPersist master, Typeable (AuthModel master))
              => HandlerT master IO (Maybe (AuthId master, AuthModel master))
maybeAuthPair = runMaybeT $ do
    aid <- MaybeT maybeAuthId
    ae  <- MaybeT $ cachedAuth aid
    return (aid, ae)

-- | Class which states that the given site is an instance of @YesodAuth@
-- and that its @AuthId@ is a lookup key for the full user information in
-- a @YesodPersist@ database.
--
-- The default implementation of @getAuthModel@ assumes that the @AuthId@
-- for the @YesodAuth@ superclass is in fact a persistent @Key@ for the
-- given value.  This is the common case in Yesod, and means that you can
-- easily look up the full information on a given user.
--
-- Since 1.4.0
class (YesodAuth master, YesodRelational master) => YesodAuthRelational master where
    type AuthModel master :: *

    getAuthModel :: AuthId master -> HandlerT master IO (Maybe (AuthModel master))

-- | Similar to 'maybeAuth', but redirects to a login page if user is not
-- authenticated or responds with error 401 if this is an API client (expecting JSON).
requireAuth :: ( YesodAuthPersist master
               , val ~ AuthModel master
               , Key val ~ AuthId master
               , PersistEntity val
               , Typeable val
               ) => HandlerT master IO (Entity val)
requireAuth = maybeAuth >>= maybe handleAuthLack return

handleAuthLack :: Yesod master => HandlerT master IO a
handleAuthLack = do
    aj <- acceptsJson
    if aj then notAuthenticated else redirectLogin

-- | Similar to 'requireAuth', but not tied to Persistent's 'Entity' type.
-- Instead, the 'AuthId' and 'AuthModel' are returned in a tuple.
requireAuthPair :: (YesodAuthPersist master, Typeable (AuthModel master))
                => HandlerT master IO (AuthId master, AuthModel master)
requireAuthPair = maybeAuthPair >>= maybe handleAuthLack return
