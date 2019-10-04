--  --                                                          ; {{{1
--
--  File        : Koneko/Bltn.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-03
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko.Bltn (initCtx) where

import Koneko.Data

initCtx :: Context -> IO Context
initCtx ctxPrim = do
  ctxBltn <- forkContext bltnModule ctxPrim
  -- ...
  return ctxBltn

-- ...

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
