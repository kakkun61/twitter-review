module Slack
    ( post
    , commented
    , candidatePosted
    , tweeted
    ) where

import Prelude
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as W
import Data.Char
import Data.Monoid
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import Model.Table.Tweet (Tweet)
import qualified Model.Table.Tweet as Tweet
import Model.Table.User (User)
import qualified Model.Table.User as User

post :: String -> Aeson.Value -> IO ()
post uri body =
    void $ W.post uri ["payload" := Aeson.encode body]

commented :: User -> Tweet -> Text -> Aeson.Value
commented user tweet comment =
    let
        author = User.emailUser user
        tid = Tweet.id tweet
    in
        Aeson.object
            [ "attachments" .= Vector.singleton (Aeson.object
                  [ "pretext" .= ("<@" <> author <> "> has commented")
                  , "author_name" .= author
                  , "title" .= ("ID: " <> (show tid))
                  , "text" .= userLink comment
                  ]
              )
            ]

candidatePosted :: User -> Tweet -> Text -> Aeson.Value
candidatePosted user tweet candidate =
    let
        author = User.emailUser user
        tid = Tweet.id tweet
    in
        Aeson.object
            [ "attachments" .= Vector.singleton (Aeson.object
                  [ "pretext" .= ("<@" <> author <> "> has posted a candidate")
                  , "author_name" .= author
                  , "title" .= ("ID: " <> (show tid))
                  , "text" .= candidate
                  ]
              )
            ]

tweeted :: User -> Tweet -> String -> Aeson.Value
tweeted user tweet uri =
    let
        author = User.emailUser user
        id = Tweet.id tweet
    in
        Aeson.object
            [ "attachments" .= Vector.singleton (Aeson.object
                  [ "pretext" .= ("<@" <> author <> "> has tweeted")
                  , "author_name" .= author
                  , "title" .= ("ID: " <> (show id))
                  , "text" .= uri
                  ]
              )
            ]

userLink :: Text -> String
userLink = reverse . fixUp . T.foldl' go ([], [])
    where
        go :: (String, String) -> Char -> (String, String)
        go (r, []) '@' = (r, "@<")
        go (r, []) e = (e:r, [])
        go (r, u) e | isAlpha e || isDigit e = (r, e:u)
                    | otherwise = (e:'>':(u<>r), [])
        fixUp :: (String, String) -> String
        fixUp (r, []) = r
        fixUp (r, u) = '>':(u<>r)
