--  --                                                          ; {{{1
--
--  File        : src-unix/Koneko_utils.hs
--  Maintainer  : FC Stegerman <flx@obfusk.net>
--  Date        : 2019-10-15
--
--  Copyright   : Copyright (C) 2019  FC Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko_utils (stdinTTY) where

import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

stdinTTY :: IO Bool
stdinTTY = queryTerminal stdInput

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
