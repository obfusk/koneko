--  --                                                          ; {{{1
--
--  File        : Koneko/JSON.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-11-17
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Koneko.JSON () where

import Data.Aeson (KeyValue(..), ToJSON(..), Value(..), object)
import Data.Text.Lazy (Text)

import Koneko.Data

instance ToJSON KPrim where
  toJSON KNil             = object [tp "nil"]
  toJSON (KBool b)        = tv "bool"   b
  toJSON (KInt i)         = tv "int"    i
  toJSON (KFloat f)       = tv "float"  f
  toJSON (KStr s)         = tv "str"    s
  toJSON (KKwd (Kwd s))   = tv "kwd"    s

instance ToJSON KValue where
  toJSON (KPrim p)        = toJSON p
  toJSON (KList (List l)) = tv "list"             l
  toJSON (KIdent i)       = tv "ident"  $ unIdent i
  toJSON (KQuot i)        = tv "quot"   $ unIdent i
  toJSON (KBlock b)       = object [
      tp "block",
      "params"  .= (map unIdent $ blkParams b),
      "code"    .= blkCode b,
      "scope"   .= Null
    ]
  toJSON _                = error "runtime type"              --  TODO

tv :: ToJSON a => Text -> a -> Value
tv t v = object [tp t, "value" .= v]

tp :: KeyValue kv => Text -> kv
tp t = "type" .= t

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
