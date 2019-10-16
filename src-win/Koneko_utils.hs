--  --                                                          ; {{{1
--
--  File        : src-win/Koneko_utils.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-15
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko_utils (stdinTTY) where

import System.IO (hIsTerminalDevice, stdin)

-- NB: not portable (GHC only)
stdinTTY :: IO Bool
stdinTTY = hIsTerminalDevice stdin

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
