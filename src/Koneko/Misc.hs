--  --                                                          ; {{{1
--
--  File        : Koneko/Misc.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-17
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko.Misc (isIdent) where

import Data.Char (isNumber)
import Data.Maybe (isJust)
import Data.Text.Lazy (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
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
isIdent s = not (T.all isNumber s) && (isJust $ parseMaybe pIdent s)

-- NB: pIdent matches numbers, isIdent rejects them
pIdent :: Parser Text
pIdent = fmap T.pack $ (:) <$> hdChar <*> many tlChar
  where
    hdChar  = letterChar <|> numberChar <|>
              oneOf identSpecial <|> oneOf brackets
    tlChar  = hdChar <|> oneOf identPre

brackets, identPre, identSpecial :: [Char]
brackets      = "(){}[]"
identPre      = "'!:"
identSpecial  = "~@$%^&*-_=+|<>/?"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
