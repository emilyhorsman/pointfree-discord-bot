{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (finally)
import Control.Monad (void)
import Pointfree
import Text.Megaparsec
import qualified Data.Text as T
import qualified Discord as D

import Lib
import MessageParser


entryPointApp :: IO ()
entryPointApp = do
  discord <- loginWithToken
  finally (app discord) (D.stopDiscord discord)


-- | Event loop!
app :: Server -> IO ()
app discord =
  D.nextEvent discord >>= either print handleEvent
  where
    handleEvent (D.MessageCreate message) =
      handleMessageCreate discord message >> print message >> app discord
    handleEvent _ =
      app discord


-- | Handle any MessageCreate we get in a channel.
handleMessageCreate :: Server -> D.Message -> IO ()
handleMessageCreate discord message = do
  print $ isFromBot message
  print $ parseMessageType message
  either return (respond discord message) $
    isFromBot message >>= parseMessageType


-- | Run the parser to get a message type.
parseMessageType :: D.Message -> Either () MessageType
parseMessageType D.Message { D.messageText = message } =
  maybe (Left ()) Right $ parseMaybe messageParser message


-- | Respond to a message from a user.
respond :: Server -> D.Message -> MessageType -> IO ()
respond discord originalMessage message =
  void $ sendMessageTo discord originalMessage $ respondWith message


-- | Send content to the same Discord channel as an original message.
sendMessageTo :: Server -> D.Message -> T.Text -> IO (Either D.RestCallException D.Message)
sendMessageTo discord originalMessage text =
  let
    message = D.CreateMessage (D.messageChannel originalMessage) text
  in
    print text >> D.restCall discord message


-- | Produce response context given a message like Help or !pf.
respondWith :: MessageType -> T.Text
respondWith Help = "Hello! I'm the pointless bot. Try `!pf sum xs = foldr (+) 0 xs`\n\nFor details see: https://wiki.haskell.org/Pointfree"
respondWith (BangPointFree expr) =
  maybe "Did not understand. Try !pf help" (wrapBackticks . T.pack) (pointfree' (T.unpack expr))


main :: IO ()
main = entryPointApp
