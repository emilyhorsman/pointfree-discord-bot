{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Data.Bool (bool)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib
import qualified Discord as D
import Pointfree

entryPointApp :: IO ()
entryPointApp = do
  discord <- loginWithToken
  finally (app discord) (D.stopDiscord discord)

app :: Server -> IO ()
app discord =
  D.nextEvent discord >>= either print handleEvent
  where
    handleEvent (D.MessageCreate message) =
      handleMessageCreate discord message >> app discord
    handleEvent _ =
      app discord

handleMessageCreate :: Server -> D.Message -> IO ()
handleMessageCreate discord message =
  either return (respond discord) $ (parseMessageType <$> (isFromBot message >>= isPrompt))

data MessageType
  = Help
  | BangPointfree T.Text
  deriving Show

parseMessageType :: D.Message -> MessageType
parseMessageType _ = Help

respond :: Server -> MessageType -> IO ()
respond discord _ = return ()

isPrompt :: D.Message -> Either () D.Message
isPrompt message@(D.Message { D.messageText = t }) =
  bool (Left ()) (Right message) $ T.isPrefixOf "!pf " t

handle discord message@(D.Message { D.messageText = t }) =
  let
    expr = T.strip <$> (T.stripPrefix "!pf " t >>= T.stripPrefix "`" >>= T.stripSuffix "`")

    result = expr >>= pointfree' . T.unpack
  in
    case result of
      Nothing -> return ()
      Just msg ->
        D.restCall discord (D.CreateMessage (D.messageChannel message) (T.pack ("`" ++ msg ++ "`"))) >> return ()

main :: IO ()
main = entryPointApp
