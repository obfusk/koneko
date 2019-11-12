--  --                                                          ; {{{1
--
--  File        : Koneko/Bltn.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-11-12
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Bltn (initCtx) where

import Data.Foldable (traverse_)
import Data.Monoid ((<>))

import Koneko.Data

initCtx :: Context -> IO Context
initCtx ctxPrim = do
  ctxBltn <- forkContext bltnModule ctxPrim
  traverse_ (defPrim ctxBltn) typePreds
  -- ...
  return ctxBltn

-- ...

typePreds :: [Builtin]
typePreds = [ mkBltn (x <> "?") $ pop1push1
            $ (== x) . typeToStr . typeOf | x <- typeNames ]

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
