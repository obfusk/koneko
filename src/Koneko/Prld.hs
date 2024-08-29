-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Prld (initCtx) where

import Koneko.Data

initCtx :: Context -> (Identifier -> IO ()) -> IO ()
initCtx _ load = load "prelude"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
