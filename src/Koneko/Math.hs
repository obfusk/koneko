--  --                                                          ; {{{1
--
--  File        : Koneko/Math.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2020-02-02
--
--  Copyright   : Copyright (C) 2020  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Math (initCtx) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Foldable (traverse_)

import Koneko.Data

initCtx :: Context -> IO ()
initCtx ctxMain = do
  ctx <- forkContext "math" ctxMain
  traverse_ (defPrim ctx) [
      mkBltn "sign" $ pop1push1 $ either (int . signum) (float . signum),
      pow, op2 "**" (**), mkBltn "pi" $ const $ flip rpush1 (pi :: Double),
      op1 "exp"   exp,    op1 "log"   log,    op1 "sqrt"  sqrt,
      op1 "sin"   sin,    op1 "cos"   cos,    op1 "tan"   tan,
      op1 "asin"  asin,   op1 "acos"  acos,   op1 "atan"  atan,
      op1 "sinh"  sinh,   op1 "cosh"  cosh,   op1 "tanh"  tanh,
      op1 "asinh" asinh,  op1 "acosh" acosh,  op1 "atanh" atanh,
      op2 "atan2" atan2
    ]

op1 :: Identifier -> (Double -> Double) -> Builtin
op1 name op = mkBltn name $ pop1push1 op

op2 :: Identifier -> (Double -> Double -> Double) -> Builtin
op2 name op = mkBltn name $ pop2push1 op

pow :: Builtin
pow = mkBltn "^" $ \_ s -> do
  ((x, y), s') <- pop2' s
  when (y < 0) $ throwIO $ RangeError "negative exponent"
  rpush1 s' $ (x :: Integer) ^ (y :: Integer)

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
