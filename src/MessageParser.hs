{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module MessageParser
  ( Parser
  , MessageType(..)
  , bangParser
  , messageParser
  ) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T


type Parser = Parsec Void T.Text


data MessageType
  = Help
  | BangPointFree T.Text
  deriving Show


bangParser :: Parser ()
bangParser = string "!pf" >> space1


messageParser :: Parser MessageType
messageParser =
  bangParser >> choice
    [ Help <$ string "help"
    , BangPointFree <$> takeRest
    ]
