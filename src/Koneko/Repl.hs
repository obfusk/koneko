--  --                                                          ; {{{1
--
--  File        : Koneko/Repl.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-17
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Repl (repl, stdinTTY) where

import Control.Monad ((>=>), unless)
import Data.Bool (bool)
import Data.Text.Lazy (Text)
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, isEOFError)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import qualified Koneko.Eval as E

-- TODO: readline? or just rlwrap?

repl :: IO ()
repl = stdinTTY >>= (bool E.evalStdin loop)
  where
    loop      = prompt' ">>> " >>=
                maybe (T.putStrLn "") (\l -> process l >> loop)
    process l = unless (T.null l) (E.evalText l)              -- TODO

prompt' :: Text -> IO (Maybe Text)
prompt' x = (Just <$> prompt x) `catchIOError`
            \e -> if isEOFError e then return Nothing else ioError e

prompt :: Text -> IO Text
prompt x = do T.putStr x; hFlush stdout; T.getLine

stdinTTY = queryTerminal stdInput

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
