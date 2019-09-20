--  --                                                          ; {{{1
--
--  File        : Main.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-20
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
import Data.Version (showVersion)
import System.Console.CmdArgs hiding (args)

import qualified Data.Text.Lazy as T
import qualified System.Console.CmdArgs as CA

import Koneko.Repl (repl, stdinTTY)

import qualified Koneko.Data as D
import qualified Koneko.Eval as E
import qualified Paths_koneko as P

version :: String
version = "koneko " ++ showVersion P.version

data KonekoCmd = KonekoCmd {
  eval        :: Maybe String,
  args        :: [String],
  interactive :: Bool
} deriving (Data, Eq, Show, Typeable)

-- TODO: use args
main :: IO ()
main = do
    KonekoCmd{..} <- cmdArgs cmd
    isatty        <- stdinTTY
    ctx           <- E.initContextWithPrelude
    let st = D.emptyStack; int = when interactive . repl ctx
    case (eval, args) of
      (Nothing, [])       -> (if isatty || interactive then repl
                              else E.evalStdin) ctx st
      (Nothing, script:_) -> E.evalFile script ctx st >>= int
      (Just code, _)      -> evalString code ctx st >>= int
  where
    evalString = E.evalText "(code)" . T.pack
    cmd = KonekoCmd {
      eval        = def &= typ "CODE"
                        &= help "code to run (instead of a script)",
      args        = def &= CA.args &= typ "[SCRIPT] ARGS...",
      interactive = def &= help "force interactive mode"
    } &= program "koneko" &= summary version
      {- &= verbosity &= details ["...", "..."] -}
      &= help "..."                                           -- TODO

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
