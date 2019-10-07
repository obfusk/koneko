--  --                                                          ; {{{1
--
--  File        : Main.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-06
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Version (showVersion)
import System.Console.CmdArgs hiding (args)

import qualified Data.Text.Lazy as T
import qualified System.Console.CmdArgs as CA

import Koneko.Data (defineIn, emptyStack, true)
import Koneko.Eval (initContext, evalFile, evalStdin, evalText)
import Koneko.Repl (repl, stdinTTY)
import Koneko.Test (doctest')

import qualified Paths_koneko as P

version :: String
version = "koneko 「子猫」 " ++ showVersion P.version

data KonekoCmd = KonekoCmd {
  args        :: [String],
  eval        :: Maybe String,
  doctest     :: Bool,
  interactive :: Bool
} deriving (Data, Eq, Show, Typeable)

-- TODO: use args
main :: IO ()
main = do
    KonekoCmd{..} <- cmdArgs cmd
    if doctest then doctest' args
    else do
      isatty <- stdinTTY; ctx <- initContext
      let st = emptyStack; int = when interactive . repl ctx
      isLoud >>= flip when (defineIn ctx "__debug__" true)
      case (eval, args) of
        (Nothing, [])       -> (if isatty || interactive then repl
                                else evalStdin) ctx st
        (Nothing, script:_) -> evalFile script ctx st >>= int
        (Just code, _)      -> evalString code ctx st >>= int
  where
    evalString = evalText "(code)" . T.pack
    cmd = KonekoCmd {
      args        = def &= CA.args &= typ argSpec,
      eval        = def &= typ "CODE"
                        &= help "code to run (instead of a script)"
                        &= groupname "Flags",
      doctest     = def &= help "run doctest (instead of a script)",
      interactive = def &= help "force interactive mode (REPL)"
    } &= program "koneko" &= summary version &= verbosity
      {- &= details ... &= help "..." -}                      --  TODO
    argSpec = "SCRIPT [ARGS...] | --eval CODE [ARGS...]" <>
              " | --doctest FILE..."

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
