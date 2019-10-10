--  --                                                          ; {{{1
--
--  File        : Koneko/Read.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-09
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
import Text.Megaparsec.Char

import qualified Data.Text.Lazy as T

import Koneko.Data (Ident, Block(..), KValue(..))
import Koneko.Misc (Parser, pIdent, pIdent_, pInt, pFloat, lexeme,
                    symbol, sp)

import qualified Koneko.Data as D

-- TODO:
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

-- | NB: also matches float and int (but they match earlier)
ident = KIdent <$> ident_

quot = char '\'' >> KQuot <$> ident_

-- TODO
block = try $ KBlock <$> do
  _ <- symbol "["
  blkArgs <- concat <$> (optional $ try $ manyTill ident_ $ symbol ".")
  blkCode <- manyValuesTill $ symbol "]"
  let blkScope = Nothing in return Block{..}

-- parser: sugar --

dict, dot, bang, key :: Parser [KValue]

dict  = try $ fmap ((:[_dict]) . D.list)
      $ symbol "{" *> manyValuesTill (symbol "}")

dot   = char '.' >> _dot <$> ident_
bang  = char '!' >> (++ [_call]) . _dot <$> ident_

key = try $ do
  k <- sugarIdent ':'; v <- value_
  return $ [D.kwd k] ++ v ++ [_pair]

_dot :: Ident -> [KValue]
_dot i = [D.kwd $ D.unIdent i, _swap, _call]

-- TODO: foo() foo{} etc.

-- parser: multiple values & program --

-- | NB: match ident last
oneValue :: Parser KValue
oneValue = choice [prim, list, quot, block, ident]

value_, manyValues, program :: Parser [KValue]

value_ = choice [dict, dot, bang, key] <|> (:[]) <$> oneValue

manyValues = concat <$> many value_

manyValuesTill :: Parser a -> Parser [KValue]
manyValuesTill end = concat <$> manyTill value_ end

program = optional shebang *> sp *> manyValues <* eof

shebang :: Parser ()
shebang = void $ string "#!" >> many (satisfy (/= '\n')) >> newline

-- parser: miscellaneous --

ident_ :: Parser Ident
ident_  = lexeme $ D.ident <$> pIdent >>=
          maybe (fail "invalid ident") return

-- TODO
sugarIdent :: Char -> Parser Text
sugarIdent c = try $ do
  i <- lexeme $ pIdent_ $ Just c
  if T.last i /= c then fail "TODO" else return $ T.init i    -- safe!

_dict, _swap, _call, _pair :: KValue
(_dict, _swap, _call, _pair) =
  let idt = KIdent . fromJust . D.ident  -- safe!
  in (idt "dict", idt "swap", idt "call", idt "=>")

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
