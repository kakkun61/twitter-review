module Template where

import Data.Maybe
import Yesod.Auth

import Foundation
import Settings
import Model

headerWidget :: Maybe User -> Widget
headerWidget mUser = $(widgetFile "header")
