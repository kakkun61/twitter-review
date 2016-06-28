module Handler.Account (getAccountR) where

import Import
-- import qualified Database.Esqueleto as E
-- import qualified Data.Set as S

getAccountR :: AccountIdParam -> Handler Html
getAccountR = error "don't call me temporally"
-- getAccountR accountIdParam = do
--     user <- entityVal <$> requireAuth
--     mStatusParamText <- lookupGetParam "status"
--     case mStatusParamText of
--         Nothing -> redirect (AccountR accountIdParam, [("status", toParam POpen)])
--         Just statusParamText -> do
--             let statusParams = fromMaybe S.empty $ fromParam statusParamText
--             $(logDebug) $ "state params: " <> pack (show (statusParams :: Set StatusParam))
--             (account, rows) <- runDB $ do
--                 accountEntity <- getBy404 (UniqueAccount accountIdParam)
--                 let account = entityVal accountEntity
--                 rows <- E.select $ E.from $ \(tw `E.InnerJoin` twc `E.InnerJoin` acc) -> do
--                     E.on $
--                         tw E.^. TweetId E.==. twc E.^. TweetCandidateTweetId
--                         E.&&. tw E.^. TweetAccountId E.==. acc E.^. AccountId
--                     E.where_ $
--                         acc E.^. AccountIdent E.==. E.val accountIdParam
--                         E.&&. E.notExists (E.from $ \twca ->
--                                                E.where_ $
--                                                    twca E.^. TweetCandidateTweetId E.==. tw E.^. TweetId
--                                                    E.&&. twca E.^. TweetCandidateCreated E.>. twc E.^. TweetCandidateCreated
--                                           )
--                     return (tw, twc)
--                 return (account, rows)
--             defaultLayout $ do
--                 headerWidget $ Just user
--                 $(widgetFile "account")

data StatusParam = POpen | PTweeted | PClosed
    deriving (Eq, Show, Ord)

instance Param StatusParam where
    toParam POpen    = "open"
    toParam PTweeted = "tweeted"
    toParam PClosed  = "closed"

    fromParam "open"    = Just POpen
    fromParam "tweeted" = Just PTweeted
    fromParam "closed"  = Just PClosed
    fromParam _         = Nothing
