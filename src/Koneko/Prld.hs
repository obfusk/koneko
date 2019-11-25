--  --                                                          ; {{{1
--
--  File        : Koneko/Prld.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-11-25
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko.Prld (modFile, initCtx) where

import System.FilePath ((</>))

import Koneko.Data
import Paths_koneko (getDataFileName)

modFile :: IO FilePath
modFile = getDataFileName $ "lib" </> "prelude.knk"

initCtx :: Context -> (FilePath -> Evaluator) -> IO ()
initCtx ctx evalFile = () <$ do f <- modFile; evalFile f ctx emptyStack

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
