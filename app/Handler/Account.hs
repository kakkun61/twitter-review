module Handler.Account where

import Import
import qualified Database.Esqueleto as E

getAccountR :: AccountIdParam -> Handler Html
getAccountR accountIdParam = do
    user <- entityVal <$> requireAuth
    runDB $ do
        accountEntity <- getBy404 (UniqueAccount accountIdParam)
        let account = entityVal accountEntity
        tweetEntities <- selectList [TweetAccountId ==. entityKey accountEntity] [Desc TweetCreated]
        rows <- E.select $ E.from $ \(tw `E.InnerJoin` twc) -> do
            E.on $ tw E.^. TweetId E.==. twc E.^. TweetCandidateTweetId
            E.where_ $
                tw E.^. TweetAccountId E.==. E.val (toSqlKey accountIdParam)
                E.&&. E.notExists (E.from $ \twca ->
                                       E.where_ $
                                           twca E.^. TweetCandidateTweetId E.==. tw E.^. TweetId
                                           E.&&. twca E.^. TweetCandidateCreated E.>. twc E.^. TweetCandidateCreated
                                  )
            return (tw, twc)
        $(logDebug) $ "rows: " ++ pack (show rows)
        lift $ defaultLayout $ do
            headerWidget $ Just user
            $(widgetFile "account")
