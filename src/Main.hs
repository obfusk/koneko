--  --                                                          ; {{{1
--
--  File        : Main.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-17
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (when)
import System.Console.CmdArgs hiding (args)

import qualified System.Console.CmdArgs as CA

import Koneko.Repl (repl, stdinTTY)

import qualified Koneko.Eval as E

version = "koneko 0.0.1"

data KonekoCmd = KonekoCmd {
  eval        :: Maybe String,
  args        :: [String],
  interactive :: Bool
} deriving (Data, Eq, Show, Typeable)

main :: IO ()
main = do
    isatty        <- stdinTTY
    KonekoCmd{..} <- cmdArgs cmd
    let int = when interactive repl
    case (eval, args) of
      (Nothing, [])           -> if isatty || interactive then repl
                                 else E.evalStdin
      (Nothing, script:args') -> E.evalFile script >> int -- TODO: args'
      (Just code, args')      -> E.evalString code >> int
  where
    cmd = KonekoCmd {
      eval        = def &= typ "CODE"
                        &= help "code to run (instead of a script)",
      args        = def &= CA.args &= typ "[SCRIPT] ARGS...",
      interactive = def &= help "force interactive mode"
    } &= program "koneko" &= summary version
      {- &= verbosity &= details ["...", "..."] -}
      &= help "..."                                           -- TODO

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
