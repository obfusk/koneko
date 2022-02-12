--  --                                                          ; {{{1
--
--  File        : Koneko/Bltn.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2022-02-12
--
--  Copyright   : Copyright (C) 2022  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Koneko.Bltn (initCtx) where

import Data.Foldable (traverse_)
import Data.Maybe (fromJust) -- careful!

#if !MIN_VERSION_GLASGOW_HASKELL(8, 8, 1, 0)
import Data.Monoid ((<>))
#endif

import qualified Data.HashMap.Strict as H

import Koneko.Data
import Koneko.Misc (pInt, pFloat, parseMaybe)
import Koneko.Prim (swap)

initCtx :: Context -> Evaluator -> IO ()
initCtx ctxMain call = do
  ctx <- forkContext bltnModule ctxMain
  traverse_ (defPrim ctx) $ typePreds ++ [
      strToInt, strToFloat,
      dup, drop_, mkBltn "swap" (biRun swap), dip call,
      dollar, at, percent,
      br_pred call, br_nil call
    ]

typePreds :: [Builtin]
typePreds = [ mkBltn (x <> "?") $ pop1push1
            $ (== x) . typeToStr . typeOf | x <- typeNames ]

strToInt, strToFloat :: Builtin
strToInt    = mkBltn "str->int"   $ pop1push1 $ parseMaybe pInt
strToFloat  = mkBltn "str->float" $ pop1push1 $ parseMaybe pFloat

dup, drop_ :: Builtin
dup   = mkBltn "dup"  $ \_ s -> push' s . fst <$> pop_' s
drop_ = mkBltn "drop" $ \_ s -> snd <$> pop_' s

dip :: Evaluator -> Builtin
dip call = mkBltn "dip" $ \c s -> do
  ((x, f), s') <- pop2' s
  flip rpush1 (x :: KValue) =<< call c (push' s' f)

dollar, at, percent :: Builtin
dollar  = mkBltn "$" $ pop2push1 partial
at      = mkBltn "@" $ pop2push1 compose
percent = mkBltn "%" $ pop2push1 $ flip compose

compose, partial :: KValue -> KValue -> KValue
compose f g = block [] [KIdent $ _ID "f", KIdent $ _ID "g"]
            $ _sc [("f", f), ("g", g), ("name", KIdent $ _ID "@")]
partial x f = block [] [KQuot $ _ID "_x", KIdent $ _ID "f"]
            $ _sc [("_x", x), ("f", f), ("name", KIdent $ _ID "$")]

-- UNSAFE!
_ID :: Identifier -> Ident
_ID = fromJust . ident

_sc :: [(Identifier, KValue)] -> Maybe Scope
_sc = Just . Scope bltnModule . H.fromList

br_pred, br_nil :: Evaluator -> Builtin

br_pred call = mkBltn "~?" $ \c s0 -> do
  ((x, f, g, p), s1) <- pop4' s0
  s2 <- call c $ foldl push' s1 [x, x, p]
  (b, s3) <- pop' s2
  call c $ push' s3 $ if truthy b then f else g

br_nil call = mkBltn "~nil" $ \c s -> do
  ((x, f, g), s') <- pop3' s
  call c $ foldl push' s' $ if isNil x then [f] else [x, g]

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
