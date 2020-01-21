--  --                                                          ; {{{1
--
--  File        : Koneko/Prld.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2020-01-20
--
--  Copyright   : Copyright (C) 2020  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Prld (initCtx) where

import Koneko.Data

initCtx :: Context -> (Identifier -> IO ()) -> IO ()
initCtx _ load = load "prelude"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
