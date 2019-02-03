{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib
import Discord
import Pointfree

pointfreeApp :: IO ()
pointfreeApp = do
  token <- T.strip <$> TIO.readFile "./auth-token.secret"
  discord <- loginRestGateway (Auth token)
  finally (app discord) (stopDiscord discord)

app :: (RestChan, Gateway, z) -> IO ()
app discord = do
  e <- nextEvent discord
  case e of
    Left err ->
      print err

    Right (MessageCreate message) -> do
      print message
      respond discord message
      app discord

    _ ->
      app discord

respond discord message
  | fromBot message = return ()
  | isPrompt message = handle discord message
  | otherwise = return ()

isPrompt :: Message -> Bool
isPrompt Message { messageText = t } =
  T.isPrefixOf "!pf " t

handle discord message@(Message { messageText = t }) =
  let
    expr = T.strip <$> (T.stripPrefix "!pf " t >>= T.stripPrefix "`" >>= T.stripSuffix "`")

    result = expr >>= pointfree' . T.unpack
  in
    case result of
      Nothing -> return ()
      Just msg ->
        restCall discord (CreateMessage (messageChannel message) (T.pack ("`" ++ msg ++ "`"))) >> return ()

fromBot Message { messageAuthor = author } =
  case author of
    Webhook -> True
    User { userIsBot = isBot } -> isBot

main :: IO ()
main = pointfreeApp
