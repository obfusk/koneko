--  --                                                          ; {{{1
--
--  File        : Koneko/Bltn.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-12-11
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
import Koneko.Misc (pInt, pFloat, parseMaybe)

initCtx :: Context -> IO ()
initCtx ctxMain = do
  ctx <- forkContext bltnModule ctxMain
  traverse_ (defPrim ctx) $ typePreds ++ [strToInt, strToFloat]

typePreds :: [Builtin]
typePreds = [ mkBltn (x <> "?") $ pop1push1
            $ (== x) . typeToStr . typeOf | x <- typeNames ]

strToInt, strToFloat :: Builtin
strToInt    = mkBltn "str->int"   $ pop1push1 $ parseMaybe pInt
strToFloat  = mkBltn "str->float" $ pop1push1 $ parseMaybe pFloat

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
