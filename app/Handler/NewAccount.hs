module Handler.NewAccount where

import Import

getNewAccountR :: Handler Html
getNewAccountR = do
    defaultLayout [whamlet|
                      <p>
                          <a href=@{AccountAuthForwardR}>Twitter log-in
                  |]
