{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( Server
  , isFromBot
  , loginWithToken
  , wrapBackticks
  ) where

import Data.Bool (bool)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Discord as D


type Server = (D.RestChan, D.Gateway, [D.ThreadIdType])


loginWithToken :: IO Server
loginWithToken =
  T.strip <$> TIO.readFile "./auth-token.secret" >>= D.loginRestGateway . D.Auth


isFromBot :: D.Message -> Either () D.Message
isFromBot message@D.Message { D.messageAuthor = author } =
  case author of
    D.Webhook -> Left ()
    D.User { D.userIsBot = isBot } ->
      bool (Right message) (Left ()) isBot


wrapBackticks :: T.Text -> T.Text
wrapBackticks x = T.concat ["`", x, "`"]
