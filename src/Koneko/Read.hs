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

module Koneko.Read (read, read') where

import Data.Char (isSpace)
import Data.Functor
import Data.Text.Lazy (Text)
import Data.Void (Void)
import Prelude hiding (lookup, quot, read)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, space1)

import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec.Char.Lexer as L

import Koneko.Data (Kwd(..), KPrim(..), KValue(..))
import Koneko.Misc (pIdent)

import qualified Koneko.Data as D

type Parser = Parsec Void Text

-- TODO:
--  * whitespace incl ,
--  * Nil, Bool, Int, Float, Str, Kwd, Rx
--  * Ident, Quoted Ident
--  * Pair, List, Dict
--  * Record
--  * Block, RawBlock vs Quoted Block
--  * ~sugar~
--
--  * parser labels

read :: Text -> [KValue]
read = read' "(read)"

read' :: FilePath -> Text -> [KValue]
read' f code
  = either (error . errorBundlePretty) id $ parse program f code

-- parser: KPrim --

nil, bool, int, float, str, kwd :: Parser KPrim

nil = KNil <$ string "nil"

bool = (KBool False <$ string "#f") <|>
       (KBool True  <$ string "#t")

int = KInt <$> (dec <|> hex <|> bin)
  where
    dec = _sig L.decimal
    hex = string "0x" *> L.hexadecimal
    bin = string "0b" *> L.binary

float = KFloat <$> _sig L.float

str = KStr <$> _str

-- TODO: pIdent matches numbers
kwd = (KKwd . Kwd) <$> (char ':' *> (_str <|> pIdent))

_sig :: Num a => Parser a -> Parser a
_sig = L.signed $ return ()

_str :: Parser Text
_str = char '"' >> T.concat <$> manyTill (esc <|> any) (char '"')
  where
    esc = choice [ t <$ string f | (f,t) <- bsl ] <?> "escape sequence"
    any = T.singleton <$> anySingle <?> "character"
    bsl = zip D.escapeFrom D.escapeTo

-- TODO: rx

-- parser: KValue --

prim, pair, list, ident, quot, block, value :: Parser KValue

prim = KPrim <$> choice [nil, bool, int, float, str, kwd]

-- TODO
pair = empty

-- TODO
list = empty

-- TODO
ident = empty

-- TODO
quot = empty

-- TODO
block = empty

value = choice [prim, pair, list, ident, quot, block]

-- TODO
program :: Parser [KValue]
program = sp *> prim `sepBy` sp1

-- parser: utilities --

sp, sp1, spaceOrComment, space1 :: Parser ()

sp  = skipMany spaceOrComment
sp1 = skipSome spaceOrComment

spaceOrComment = space1 <|> (L.skipLineComment ";")

space1 = void $ takeWhile1P (Just "white space") isSpaceOrComma

isSpaceOrComma :: Char -> Bool
isSpaceOrComma c = isSpace c || c == ','

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
