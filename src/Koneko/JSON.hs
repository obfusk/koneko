--  --                                                          ; {{{1
--
--  File        : Koneko/JSON.hs
--  Maintainer  : FC Stegerman <flx@obfusk.net>
--  Date        : 2020-11-12
--
--  Copyright   : Copyright (C) 2020  FC Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

module Koneko.JSON (initCtx) where

import Control.Exception (throwIO)
import Data.Foldable (traverse_)

import Koneko.Data

initCtx :: Context -> IO ()
initCtx ctxMain = do
  ctx <- forkContext "json" ctxMain
  traverse_ (defPrim ctx) [jsonTo, jsonFrom]

jsonTo, jsonFrom :: Builtin
jsonTo = mkBltn "->" $ \_ s -> do
  (x, s') <- pop' s; either throwIO (rpush1 s') $ fromJSON x
jsonFrom = mkBltn "<-" $ \_ s -> do
  (x, s') <- pop' s; either throwIO (rpush1 s') $ toJSON x

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
