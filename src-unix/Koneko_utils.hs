-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Koneko_utils (stdinTTY) where

import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

stdinTTY :: IO Bool
stdinTTY = queryTerminal stdInput

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
