--  --                                                          ; {{{1
--
--  File        : Koneko/Bltn.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2020-01-31
--
--  Copyright   : Copyright (C) 2020  Felix C. Stegerman
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
  --  ++ [dup, drop_]

typePreds :: [Builtin]
typePreds = [ mkBltn (x <> "?") $ pop1push1
            $ (== x) . typeToStr . typeOf | x <- typeNames ]

strToInt, strToFloat :: Builtin
strToInt    = mkBltn "str->int"   $ pop1push1 $ parseMaybe pInt
strToFloat  = mkBltn "str->float" $ pop1push1 $ parseMaybe pFloat

-- dup, drop_ :: Builtin
-- dup   = mkBltn "dup"  $ \_ s -> push' s . fst <$> pop_' s
-- drop_ = mkBltn "drop" $ \_ s -> snd <$> pop_' s

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
