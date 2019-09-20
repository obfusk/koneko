--  --                                                          ; {{{1
--
--  File        : Koneko/Read.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-20
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

                                                              --  {{{1
-- |
--
-- >>> :set -XOverloadedStrings
-- >>> read "nil #f 42 3.14 \"Hello, World!\" :foo foo"
-- [nil,#f,42,3.14,"Hello, World!",:foo,foo]
--
-- ... TODO ...
--

                                                              --  }}}1

module Koneko.Read (read, read') where

import Data.Char (isSpace)
import Data.Functor
import Data.Maybe (fromJust)
import Data.Text.Lazy (Text)
import Prelude hiding (lookup, quot, read)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, space1)

import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec.Char.Lexer as L

import Koneko.Data (Kwd(..), List(..), KPrim(..), KValue(..))
import Koneko.Misc (Parser, pIdent, pInt, pFloat)

import qualified Koneko.Data as D

-- TODO:
--  * Dict
--  * Record
--  * RawBlock vs Quoted Block
--  * ~sugar~
--  * parser labels

read :: Text -> [KValue]
read = read' "(read)"

read' :: FilePath -> Text -> [KValue]
read' f code
  = either (error . errorBundlePretty) id $ parse program f code

-- parser: KPrim --

nil, bool, int, float, str, kwd :: Parser KPrim

bool = (KBool False <$ string "#f") <|>
       (KBool True  <$ string "#t")

nil   = KNil        <$ string "nil"
int   = KInt        <$> pInt
float = KFloat      <$> pFloat
str   = KStr        <$> _str
kwd   = KKwd . Kwd  <$> (char ':' *> (_str <|> pIdent))

_str :: Parser Text
_str = char '"' >> T.concat <$> manyTill (esc <|> chr) (char '"')
  where
    esc = choice [ t <$ string f | (f,t) <- bsl ] <?> "escape sequence"
    chr = T.singleton <$> anySingle <?> "character"
    bsl = zip D.escapeFrom D.escapeTo

-- TODO: rx

-- parser: KValue --

prim, list, ident, quot, block, value :: Parser KValue

prim = KPrim <$> choice prims
  where
    prims = map (try . lexeme) [nil, int, float] ++
            map        lexeme  [bool, str, kwd]

-- TODO
list = empty {- KList . List <$> (a <|> b)
  where
    a = [] <$ string "()"
    b = between (string "(") (string ")") values' -}

ident = lexeme $ KIdent . fromJust . D.ident <$> pIdent

-- TODO
quot = empty

-- TODO
block = empty

value = choice [prim, list, ident, quot, block]

program :: Parser [KValue]
program = sp *> many value <* eof

-- parser: utilities --

lexeme :: Parser a -> Parser a
lexeme p = p <* (sp1 <|> eof)

symbol :: Text -> Parser Text
symbol = lexeme . string

sp, sp1, spaceOrComment, space1 :: Parser ()

sp  = skipMany spaceOrComment
sp1 = skipSome spaceOrComment

spaceOrComment = space1 <|> (L.skipLineComment ";")

space1 = void $ takeWhile1P (Just "white space") isSpaceOrComma

isSpaceOrComma :: Char -> Bool
isSpaceOrComma c = isSpace c || c == ','

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
