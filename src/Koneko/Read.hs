--  --                                                          ; {{{1
--
--  File        : Koneko/Read.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-17
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko.Read (read, read') where

import Data.Text.Lazy (Text)
-- import Data.Void (Void)
import Prelude hiding (read)
-- import Text.Megaparsec hiding (State)

import Koneko.Data

-- type Parser = Parsec Void Text

-- TODO:
--  * whitespace incl ;
--  * Nil, Bool, Int, Float, Str, Kwd, Rx
--  * Ident, Quoted Ident
--  * Pair, List, Dict
--  * Record
--  * Block, RawBlock vs Quoted Block
--  * ~sugar~

-- TODO
read :: Text -> [KValue]
read _ = error "read not yet implemented"

-- TODO
read' :: FilePath -> Text -> [KValue]
read' _ _ = error "read' not yet implemented"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
