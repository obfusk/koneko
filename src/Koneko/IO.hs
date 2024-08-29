-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module Koneko.IO (initCtx) where

import Control.Exception (throwIO)
import Data.Foldable (traverse_)
import System.IO.Error (tryIOError)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Koneko.Data

initCtx :: Context -> IO ()
initCtx ctxMain = do
  ctx <- forkContext "io" ctxMain
  traverse_ (defPrim ctx) [ioContents, ioLines]

ioContents, ioLines :: Builtin

ioContents = mkBltn "contents!" $ \_ s -> do
    (x, s') <- pop' s
    (tryIOError $ T.readFile $ T.unpack x) >>= either f (rpush1 s')
  where
    f = throwIO . Fail . ("io.contents!: " ++) . show

ioLines = mkBltn "lines!" $ \_ s -> do
    (x, s') <- pop' s
    (tryIOError $ fmap (map str . T.lines) $ T.readFile $ T.unpack x)
      >>= either f (rpush1 s')
  where
    f = throwIO . Fail . ("io.lines!: " ++) . show

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
