module Handler.Account where

import Import
import qualified Database.Esqueleto as E

getAccountR :: AccountIdParam -> Handler Html
getAccountR accountIdParam = do
    user <- entityVal <$> requireAuth
    runDB $ do
        accountEntity <- getBy404 (UniqueAccount accountIdParam)
        let account = entityVal accountEntity
        rows <- E.select $ E.from $ \(tw `E.InnerJoin` twc `E.InnerJoin` acc `E.InnerJoin` usr) -> do
            E.on $
                tw E.^. TweetId E.==. twc E.^. TweetCandidateTweetId
                E.&&. tw E.^. TweetAccountId E.==. acc E.^. AccountId
                E.&&. twc E.^. TweetCandidateUserId E.==. usr E.^. UserId
            E.where_ $
                acc E.^. AccountIdent E.==. E.val accountIdParam
                E.&&. E.notExists (E.from $ \twca ->
                                       E.where_ $
                                           twca E.^. TweetCandidateTweetId E.==. tw E.^. TweetId
                                           E.&&. twca E.^. TweetCandidateCreated E.>. twc E.^. TweetCandidateCreated
                                  )
            return (tw, twc, usr)
        $(logDebug) $ "rows: " ++ pack (show rows)
        lift $ defaultLayout $ do
            headerWidget $ Just user
            $(widgetFile "account")
