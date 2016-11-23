module Handler.Account (getAccountR) where

import Import hiding (on)
-- import qualified Database.Esqueleto as E
import Yesod.Core.Handler (notFound)
import qualified Data.Set as S
import Database.Relational.Query (on)
import qualified Model.Table.User as User
import qualified Model.Table.Account as Account
import qualified Model.Table.Tweet as Tweet
import qualified Model.Table.TweetCandidate as TweetCandidate

getAccountR :: AccountIdParam -> Handler Html
-- getAccountR = error "don't call me temporally"
getAccountR accountIdParam = do
    user <- requireAuth
    mStatusParamText <- lookupGetParam "status"
    case mStatusParamText of
        Nothing -> redirect (AccountR accountIdParam, [("status", toParam POpen)])
        Just statusParamText -> do
            let statusParams = fromMaybe S.empty $ fromParam statusParamText
            $(logDebug) $ "state params: " <> pack (show (statusParams :: Set StatusParam))
            (account, rows) <- runRelational $ do
                accounts <- flip runQuery () $ relationalQuery $ relation $ do
                                a <- query Account.account
                                wheres $ a ! Account.id' .=. value accountIdParam
                                return a
                case accounts of
                    [account] -> do
                        rows <- flip runQuery () $ relationalQuery $ relation $ do
                                    tw <- query Tweet.tweet
                                    twc <- query TweetCandidate.tweetCandidate
                                    acc <- query Account.account
                                    on $ tw ! Tweet.id' .=. twc ! TweetCandidate.tweetId'
                                    on $ tw ! Tweet.accountId' .=. acc ! Account.id'
                                    wheres $ acc ! Account.id' .=. value accountIdParam
                                    newer <- queryList $ relation $ do
                                                 twca <- query TweetCandidate.tweetCandidate
                                                 wheres $ twca ! TweetCandidate.tweetId' .=. tw ! Tweet.id'
                                                 wheres $ twca ! TweetCandidate.created' .>. twc ! TweetCandidate.created'
                                                 return $ value (1 :: Int64)
                                    wheres $ not' $ exists newer
                                    return $ (,) |$| tw |*| twc
                        return (account, rows)
                    _ -> lift notFound
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
            defaultLayout $ do
                headerWidget $ Just user
                $(widgetFile "account")

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
