module Handler.Account (getAccountR) where

import Import hiding (on)
import qualified Data.Set as S
import Data.Foldable (foldr1)
import Database.Relational.Query (on, or')
import qualified Model.Table.Account as Account
import qualified Model.Table.Tweet as Tweet
import qualified Model.Table.TweetCandidate as TweetCandidate

getAccountR :: AccountIdParam -> Handler Html
getAccountR accountIdParam = do
    user <- requireAuth
    mStatusParamText <- lookupGetParam "status"
    case mStatusParamText of
        Nothing -> redirect (AccountR accountIdParam, [("status", toParam POpen)])
        Just statusParamText -> do
            let statusParams = fromMaybe S.empty $ fromParam statusParamText
            (account, rows) <- runRelational $ do
                accounts <- flip runQuery () $ relationalQuery $ relation $ do
                                a <- query Account.account
                                wheres $ a ! Account.id' .=. value accountIdParam
                                return a
                case accounts of
                    [account] -> do
                        if S.null statusParams
                            then return (account, [])
                            else do
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
                                            wheres $ foldr1 or' $ map (\s -> tw ! Tweet.status' .=. value (convert s)) $ S.toList statusParams
                                            return $ (,) |$| tw |*| twc
                                return (account, rows)
                    _ -> lift notFound
            defaultLayout $ do
                headerWidget $ Just user
                accountWidget account rows statusParams
