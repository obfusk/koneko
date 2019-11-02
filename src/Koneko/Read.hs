--  --                                                          ; {{{1
--
--  File        : Koneko/Read.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-11
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
import Control.Monad (replicateM)
import Control.Monad.Fail (MonadFail)
import Data.Functor
import Data.List (foldl')
import Data.Maybe (fromJust) -- careful!
import Data.Monoid((<>))
import Data.Text.Lazy (Text)
import Prelude hiding (quot, read)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Char as C
import qualified Data.Text.Lazy as T

import Koneko.Data (Identifier, Ident, Block(..), KValue(..))
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
_str  = char '"' >> T.concat <$> manyTill (choice [
          esc, hex "\\x" 2, hex "\\u" 4, hex "\\U" 8, chr
        ]) (char '"')
  where
    esc     = choice [ t <$ string f | (f,t) <- bsl ] <?> "escape sequence"
    chr     = T.singleton <$> anySingle <?> "character"
    bsl     = zip D.escapeFrom D.escapeTo
    hex p n = string p >> T.singleton <$> _hex n

_hex :: Int -> Parser Char
_hex n = C.chr . foldl' (\a c -> a * 16 + C.digitToInt c) 0 <$>
         replicateM n (satisfy C.isHexDigit <?> "hex digit")

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

dict, dot, bang, key, apply, applyDict :: Parser [KValue]

dict  = try $ fmap ((:[_dict]) . D.list)
      $ symbol "{" *> manyValuesTill (symbol "}")

dot   = char '.' >> _dot <$> ident_
bang  = char '!' >> (++ [_call]) . _dot <$> ident_

key = try $ do
  k <- sugarIdent ':'; v <- value_
  return $ [D.kwd k] ++ v ++ [_pair]

apply = try $ do
  (q, l) <- _ap '(' ")"; return $ [l, q, _apply]

applyDict = try $ do
  (q, l) <- _ap '{' "}"; return $ [l, _dict, q, _applyDict]

_dot :: Ident -> [KValue]
_dot i = [D.kwd $ D.unIdent i, _swap, _call]

_ap :: Char -> Text -> Parser (KValue, KValue)
_ap op cl = do
  i <- sugarIdent op; vs <- manyValuesTill $ symbol cl
  q <- KQuot <$> identOrFail i
  return (q, D.list vs)

sugar :: Parser [KValue]
sugar = choice [dict, dot, bang, key, apply, applyDict]

-- parser: multiple values & program --

-- | NB: match ident last
oneValue :: Parser KValue
oneValue = choice [prim, list, quot, block, ident]

value_, manyValues, program :: Parser [KValue]

value_ = sugar <|> (:[]) <$> oneValue

manyValues = concat <$> many value_

program = optional shebang *> sp *> manyValues <* eof

manyValuesTill :: Parser a -> Parser [KValue]
manyValuesTill end = concat <$> manyTill value_ end

shebang :: Parser ()
shebang = void $ string "#!" >> many (satisfy (/= '\n')) >> newline

-- parser: miscellaneous --

ident_ :: Parser Ident
ident_ = lexeme $ pIdent >>= identOrFail

-- TODO
sugarIdent :: Char -> Parser Identifier
sugarIdent c = try $ do
  i <- lexeme $ pIdent_ $ Just c
  if T.last i /= c then fail "TODO" else return $ T.init i    -- safe!

_dict, _swap, _call, _pair, _apply, _applyDict :: KValue
(_dict, _swap, _call, _pair, _apply, _applyDict) =
  let i = KIdent . fromJust . D.ident . (<> "__") . ("__" <>) -- safe!
  in (i "dict", i "swap", i "call", i "=>", i "apply", i "apply-dict")

-- miscellaneous --

identOrFail :: MonadFail m => Identifier -> m Ident
identOrFail = maybe (fail "invalid ident") return . D.ident

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
