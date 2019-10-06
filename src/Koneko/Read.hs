--  --                                                          ; {{{1
--
--  File        : Koneko/Read.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-06
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

                                                              --  {{{1
-- |
--
-- >>> :set -XOverloadedStrings
-- >>> x = read "nil #f 42 3.14 \"Hello, World!\" :foo foo"
-- >>> x
-- [nil,#f,42,3.14,"Hello, World!",:foo,foo]
-- >>> map D.typeOf x
-- [#<::nil>,#<::bool>,#<::int>,#<::float>,#<::str>,#<::kwd>,#<::ident>]
--
-- >>> x = read "( 1 2 :foo ) 'foo [ x . 'x 'x ]"
-- >>> x
-- [( 1 2 :foo ),'foo,[ x . 'x 'x ]]
-- >>> map D.typeOf x
-- [#<::list>,#<::quot>,#<::block>]
--
-- ... TODO ...
--

                                                              --  }}}1

module Koneko.Read (read, read') where

import Control.Exception (throw)
import Data.Functor
import Data.Maybe (fromJust) -- careful!
import Data.Text.Lazy (Text)
import Prelude hiding (quot, read)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, space1)

import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec.Char.Lexer as L

import Koneko.Data (Ident, Block(..), KValue(..))
import Koneko.Misc (Parser, pIdent, brackets, pInt, pFloat,
                    isSpaceOrComma)

import qualified Koneko.Data as D

-- TODO:
--  * Dict
--  * Record
--  * RawBlock vs Quoted Block
--  * ~sugar~
--  * parser labels
--  * test corner cases & failures

read :: Text -> [KValue]
read = read' "(read)"

read' :: FilePath -> Text -> [KValue]
read' f code  = either (throw . D.ParseError . errorBundlePretty) id
              $ parse program f code

-- parser: primitives --

nil, bool, int, float, str, kwd :: Parser KValue

bool = (D.false <$ string "#f") <|>
       (D.true  <$ string "#t")

nil   = D.nil   <$ string "nil"
int   = D.int   <$> pInt
float = D.float <$> pFloat
str   = D.str   <$> _str
kwd   = D.kwd   <$> (char ':' *> (_str <|> pIdent))

_str :: Parser Text
_str = char '"' >> T.concat <$> manyTill (esc <|> chr) (char '"')
  where
    esc = choice [ t <$ string f | (f,t) <- bsl ] <?> "escape sequence"
    chr = T.singleton <$> anySingle <?> "character"
    bsl = zip D.escapeFrom D.escapeTo

-- TODO: rx

-- parser: values --

prim, list, ident, quot, block :: Parser KValue

prim = choice $ map (try . lexeme) [nil, int, float] ++
                map        lexeme  [bool, str, kwd]

list = try $ D.list <$> (a <|> b)
  where
    a = [] <$ symbol "()"
    b = symbol "(" *> manyValuesTill (symbol ")")

-- TODO: dict

-- | NB: also matches float and int (but they match earlier)
ident = KIdent <$> ident_

ident_ :: Parser Ident
ident_ = notFollowedBy (lexeme $ oneOf brackets) *> idt
  where
    idt = lexeme $ D.ident <$> pIdent >>=
          maybe (fail "invalid ident") return

quot = char '\'' >> KQuot <$> ident_

-- TODO
block = try $ KBlock <$> do
  _ <- symbol "["
  blkArgs <- concat <$> (optional $ try $ manyTill ident_ $ symbol ".")
  blkCode <- manyValuesTill $ symbol "]"
  let blkScope = Nothing in return Block{..}

-- parser: sugar --

dot, bang :: Parser [KValue]

dot     = char '.' >> _dot <$> ident_
bang    = char '!' >> (++ [_call]) . _dot <$> ident_

_dot :: Ident -> [KValue]
_dot i  = [D.kwd $ D.unIdent i, _swap, _call]

_swap, _call :: KValue
_swap   = KIdent $ fromJust $ D.ident "swap" -- safe!
_call   = KIdent $ fromJust $ D.ident "call" -- safe!

-- TODO: foo() foo{} etc.

-- parser: multiple values & program --

-- | NB: match ident last
oneValue :: Parser KValue
oneValue = choice [prim, list, quot, block, ident]

value_, manyValues, program :: Parser [KValue]

value_ = (:[]) <$> oneValue <|> choice [dot, bang]

manyValues = concat <$> many value_

manyValuesTill :: Parser a -> Parser [KValue]
manyValuesTill end = concat <$> manyTill value_ end

program = optional shebang *> sp *> manyValues <* eof

shebang :: Parser ()
shebang = void $ string "#!" >> many (satisfy (/= '\n')) >> newline

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

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
