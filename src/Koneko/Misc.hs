-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Misc (
  Parser, isIdent, pIdent, pIdent_, pInt, pFloat, isSpaceOrComma,
  lexeme, symbol, speof, sp, sp1, spaceOrComment, prompt, firstJust,
  parseMaybe
) where

import Data.Char (isSpace)
import Data.Functor
import Data.List ((\\))
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Data.Void (Void)
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, isEOFError)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, space1)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L

-- parser: common --

type Parser = Parsec Void Text

                                                              --  {{{1
-- | Is the string an identifier?
--
-- NB: only partially checks whether it is a *valid* identifier (i.e.
-- whether it is not e.g. "nil").
--
-- >>> :set -XOverloadedStrings
-- >>> isIdent "nil"  -- OOPS
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
-- >>> isIdent "["
-- False
-- >>> isIdent "]"
-- False
-- >>> isIdent "x]"
-- True
-- >>> isIdent "x["
-- False
-- >>> isIdent "x:"
-- False
--

                                                              --  }}}1
isIdent :: Text -> Bool
isIdent s = parses pIdent s && not (parses pInt s || parses pFloat s)

-- | NB: also matches float and int
pIdent :: Parser Text
pIdent = pIdent_ Nothing

-- TODO
pIdent_ :: Maybe Char-> Parser Text
pIdent_ ok = T.pack <$> (try a <|> b)
  where
    a       = (:[]) <$> okChar <* notFollowedBy miChar
    b       = (:)   <$> hdChar <*> some (try b1 <|> tlChar)
    b1      = miChar <* notFollowedBy (speof <|> (bad >> speof))
    okChar  = letterChar <|> numberChar <|> symbolChar <|> oneOf specialChar
    hdChar  = okChar <|> oneOf brackets
    miChar  = hdChar <|> oneOf badStart
    tlChar  = okChar <|> oneOf bracketsC <|> good
    bad     = oneOf $ badEnd   \\ maybeToList ok
    good    = oneOf $ goodTail ++ maybeToList ok

brackets, bracketsO, bracketsC, specialChar, badStart, goodTail,
  badEnd :: [Char]

brackets      = bracketsO ++ bracketsC
bracketsO     = "({["
bracketsC     = ")}]"
specialChar   = "@%&*-_/?"
badStart      = goodTail ++ ":"
goodTail      = "'!"
badEnd        = bracketsO ++ ":"

pInt :: Parser Integer
pInt = hex <|> bin <|> dec
  where
    hex = string "0x" *> L.hexadecimal
    bin = string "0b" *> L.binary
    dec = signed L.decimal

pFloat :: Parser Double
pFloat = signed L.float

-- parser: helpers --

signed :: Num a => Parser a -> Parser a
signed = L.signed $ return ()

isSpaceOrComma :: Char -> Bool
isSpaceOrComma c = isSpace c || c == ','

parses :: Parser a -> Text -> Bool
parses p = isJust . parseMaybe p

-- parser: utilities --

lexeme :: Parser a -> Parser a
lexeme p = p <* speof

symbol :: Text -> Parser Text
symbol = lexeme . string

speof, sp, sp1, spaceOrComment, space1 :: Parser ()

speof = sp1 <|> eof
sp    = skipMany spaceOrComment
sp1   = skipSome spaceOrComment

spaceOrComment = space1 <|> (L.skipLineComment ";")

space1 = void $ takeWhile1P (Just "white space") isSpaceOrComma

-- utilities --

prompt :: Maybe Text -> IO (Maybe Text)
prompt x = (Just <$> f) `catchIOError` g
  where
    f   = maybe (return ()) h x >> T.getLine
    g e = if isEOFError e then return Nothing else ioError e
    h s = do T.putStr s; hFlush stdout

-- miscellaneous --

firstJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJust []      = return Nothing
firstJust (x:xt)  = x >>= maybe (firstJust xt) (return . Just)

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
