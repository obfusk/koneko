-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
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
import Data.Functor
import Data.Maybe (fromJust) -- careful!
import Data.Text (Text)
import Prelude hiding (quot, read)
import Text.Megaparsec
import Text.Megaparsec.Char

#if !MIN_VERSION_GLASGOW_HASKELL(8, 8, 1, 0)
import Control.Monad.Fail (MonadFail)
import Data.List (init)
#endif
#if !MIN_VERSION_GLASGOW_HASKELL(9, 10, 1, 0)
import Data.List (foldl')
#endif

import qualified Data.Char as C
import qualified Data.Text as T

import Koneko.Data (Identifier, Ident, Block(..), KValue(..))
import Koneko.Misc (Parser, pIdent, pIdent_, pInt, pFloat, lexeme,
                    symbol, sp)

import qualified Koneko.Data as D

-- TODO:
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
    esc     = choice [ T.singleton t <$ string f | (f,t) <- bsl ] <?> "escape sequence"
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
quot  = char '\'' >> KQuot <$> ident_
block = KBlock <$> block_

-- TODO
block_ :: Parser Block
block_ = try $ do
  _       <- symbol "["
  params  <- concat <$> (optional $ try $ manyTill ident_ $ symbol ".")
  code    <- manyValuesTill $ symbol "]"
  return $ Block params code Nothing

-- parser: sugar --

ellipsis, modid, qmodid, qhole, dhole, qdig, ddig, qdot, qbang, dot,
  bang, dict, key, apply, applyDict, idblk :: Parser [KValue]

ellipsis = [_IDENT "ellipsis"] <$ symbol "..."

modid = try $ do
  m <- D.kwd . D.unIdent <$> identNL <* char '.'
  i <- D.kwd . D.unIdent <$> ident_
  return [i, m, _IDENT "module-get", _IDENT "call"]

qmodid = try $ char '\'' >> init <$> modid                    -- safe!

-- TODO
qhole = try $ char '\'' >>                         _wrap  <$> block_
dhole = try $ char '.'  >> ((++ [_IDENT "call"]) . _wrap) <$> block_

qdig  = try $ char '\'' >> _dig (KQuot . _IDENT')             -- safe!
ddig  = try $ char '.'  >> _dig          _IDENT               -- safe!

qdot  = string    "'."  >> _blk <$> _isc []
qbang = string    "'!"  >> _blk <$> _isc [_IDENT "call"]

dot   = char       '.'  >>          _isc []
bang  = char       '!'  >>          _isc [_IDENT "call"]

dict  = try $ fmap ((:[_IDENT "dict"]) . D.list)
      $ symbol "{" *> manyValuesTill (symbol "}")

key = try $ do
  k <- sugarIdent ':'; v <- value_
  return $ [D.kwd k] ++ v ++ [_IDENT "=>"]

apply = try $ do
  (q, l) <- _ap '(' ")"; return [l, q, _IDENT "apply"]

applyDict = try $ do
  (q, l) <- _ap '{' "}"
  return [l, _IDENT "dict", q, _IDENT "apply-dict"]

idblk = try $ do i <- identNL; b <- block_; return [KBlock b, KIdent i]

_wrap :: Block -> [KValue]
_wrap b = [D.block (D.digitParams b) [KBlock b] Nothing]

_dig :: (Text -> KValue) -> Parser [KValue]
_dig f  = fmap ((:[]) . f . T.singleton)
        $ lexeme $ satisfy (`elem` ['1'..'9'])

_blk :: [KValue] -> [KValue]
_blk code = [D.block [] code Nothing]

_isc :: [KValue] -> Parser [KValue]
_isc vs = f <$> ident_
  where
    f i = [D.kwd $ D.unIdent i, _IDENT "swap", _IDENT "call"] ++ vs

_ap :: Char -> Text -> Parser (KValue, KValue)
_ap op cl = do
  i <- sugarIdent op; vs <- manyValuesTill $ symbol cl
  q <- KQuot <$> identOrFail i
  return (q, D.list vs)

sugar :: Parser [KValue]
sugar = choice [
    ellipsis, modid, qmodid, qhole, dhole, qdig, ddig, qdot, qbang,
    dot, bang, dict, key, apply, applyDict, idblk
  ]

-- parser: multiple values & program --

-- | NB: match ident last
oneValue :: Parser KValue
oneValue = choice [prim, list, quot, block, ident]

value_, manyValues, program :: Parser [KValue]

value_      = sugar <|> (:[]) <$> oneValue
manyValues  = concat <$> many value_
program     = optional shebang *> sp *> manyValues <* eof

manyValuesTill :: Parser a -> Parser [KValue]
manyValuesTill end = concat <$> manyTill value_ end

shebang :: Parser ()
shebang = void $ string "#!" >> many (satisfy (/= '\n')) >> newline

-- parser: miscellaneous --

ident_, identNL :: Parser Ident
ident_  = lexeme $ identNL
identNL = pIdent >>= identOrFail

-- TODO
sugarIdent :: Char -> Parser Identifier
sugarIdent c = try $ do
  i <- lexeme $ pIdent_ $ Just c
  if T.last i /= c then fail "TODO" else return $ T.init i    -- safe!

-- miscellaneous --

-- UNSAFE!
_IDENT  :: Text -> KValue
_IDENT' :: Text -> Ident
_IDENT  = KIdent . _IDENT'
_IDENT' = fromJust . D.ident . D.underscored

identOrFail :: MonadFail m => Identifier -> m Ident
identOrFail = maybe (fail "invalid ident") return . D.ident

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
