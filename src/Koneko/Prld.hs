--  --                                                          ; {{{1
--
--  File        : Koneko/Prld.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-03
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko.Prld (modFile, initCtx) where

import Koneko.Data
import Paths_koneko (getDataFileName)

import qualified Koneko.Prim as Prim

modFile :: IO FilePath
modFile = getDataFileName "lib/prelude.knk"

initCtx :: Context -> IO Context
initCtx ctxBltn = do
  ctxPrld <- forkContext prldModule ctxBltn
  -- ...
  return ctxPrld

-- ...

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
