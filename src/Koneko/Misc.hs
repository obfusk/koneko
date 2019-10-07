--  --                                                          ; {{{1
--
--  File        : Koneko/Misc.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-07
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Misc (
  Parser, isIdent, pIdent, brackets, pInt, pFloat, isSpaceOrComma,
  prompt', prompt
) where

import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Text.Lazy (Text)
import Data.Void (Void)
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, isEOFError)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

                                                              --  {{{1
-- | Is the string an identifier?
--
-- NB: does not check whether it is a *valid* identifier (i.e. whether
-- it is not e.g. [ or ) or nil).
--
-- >>> :set -XOverloadedStrings
-- >>> isIdent "nil"
-- True
-- >>> isIdent ""
-- False
-- >>> isIdent "42"
-- False
-- >>> isIdent "foo-bar'"
-- True
-- >>> isIdent "[子猫]"
-- True
-- >>> isIdent "'foo"
-- False
-- >>> isIdent "@$%^&*!"
-- True

                                                              --  }}}1
isIdent :: Text -> Bool
isIdent s = parses pIdent s && not (parses pInt s || parses pFloat s)

-- | NB: also matches float and int
pIdent :: Parser Text
pIdent = T.pack <$> ((:) <$> hdChar <*> many tlChar)
  where
    hdChar  = letterChar <|> numberChar <|>
              oneOf identSpecial <|> oneOf brackets
    tlChar  = hdChar <|> oneOf identPre

brackets, identPre, identSpecial :: [Char]

brackets      = "(){}[]"
identPre      = "'!:"
identSpecial  = "~@$%^&*-_=+|<>/?"

pInt :: Parser Integer
pInt = hex <|> bin <|> dec
  where
    hex = string "0x" *> L.hexadecimal
    bin = string "0b" *> L.binary
    dec = signed L.decimal

pFloat :: Parser Double
pFloat = signed L.float

signed :: Num a => Parser a -> Parser a
signed = L.signed $ return ()

parses :: Parser a -> Text -> Bool
parses p = isJust . parseMaybe p

isSpaceOrComma :: Char -> Bool
isSpaceOrComma c = isSpace c || c == ','

-- utilities --

prompt' :: Text -> IO (Maybe Text)
prompt' x = (Just <$> prompt x) `catchIOError` \e ->
            if isEOFError e then return Nothing else ioError e

prompt :: Text -> IO Text
prompt x = do T.putStr x; hFlush stdout; T.getLine

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
