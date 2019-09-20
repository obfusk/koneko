--  --                                                          ; {{{1
--
--  File        : Koneko/Misc.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-20
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko.Misc (isIdent, pIdent, matchIf) where

import Data.Char (isNumber)
import Data.Maybe (isJust)
import Data.Text.Lazy (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text.Lazy as T

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
isIdent = isJust . parseMaybe pIdent

-- TODO: still matches -11, 4e3
pIdent :: Parser Text
pIdent = matchIf (not . T.all isNumber) _pIdent

-- NB: also matches numbers
_pIdent :: Parser Text
_pIdent = fmap T.pack $ (:) <$> hdChar <*> many tlChar
  where
    hdChar  = letterChar <|> numberChar <|>
              oneOf identSpecial <|> oneOf brackets
    tlChar  = hdChar <|> oneOf identPre

brackets, identPre, identSpecial :: [Char]
brackets      = "(){}[]"
identPre      = "'!:"
identSpecial  = "~@$%^&*-_=+|<>/?"

matchIf :: (a -> Bool) -> Parser a -> Parser a
matchIf f p = do r <- p; if f r then return r else fail "oops"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
