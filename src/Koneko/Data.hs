--  --                                                          ; {{{1
--
--  File        : Koneko/Data.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-17
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko.Data () where

import Data.Text.Lazy (Text)

import qualified Data.Text.Lazy as T

import Koneko.Misc (isIdent)

-- TODO:
--  * Ident, Quoted Ident
--  * Pair, List, Dict
--  * Record
--  * Block, RawBlock vs Quoted Block
--  * Scope (Namespace = global scope)
--  * Stack

newtype Kwd = Kwd Text
  deriving (Eq, Ord)

-- Todo: Rx
data KPrim
    = KNil
    | KBool Bool
    | KInt Integer
    | KFloat Double
    | KStr Text
    | KKwd Kwd
  deriving (Eq, Ord)

-- TODO
data KValue
    = KPrim KPrim
  deriving (Eq, Ord)

instance Show Kwd where
  show (Kwd x) = ":" ++ showIdent x

instance Show KPrim where
  show KNil       = "nil"
  show (KBool b)  = if b then "#t" else "#f"
  show (KInt i)   = show i
  show (KFloat f) = show f
  show (KStr s)   = show s
  show (KKwd k)   = show k

-- TODO
instance Show KValue where
  show (KPrim p)  = show p

showIdent :: Text -> String
showIdent s = if isIdent s then T.unpack s else show s

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
