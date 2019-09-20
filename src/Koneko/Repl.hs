--  --                                                          ; {{{1
--
--  File        : Koneko/Repl.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-20
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Repl (repl, stdinTTY) where

import Control.Monad (unless)
import Data.Bool (bool)
import Data.Text.Lazy (Text)
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, isEOFError)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import qualified Koneko.Data as D
import qualified Koneko.Eval as E

-- TODO: readline? or just rlwrap?

repl :: D.Context -> D.Stack -> IO ()
repl ctx st = stdinTTY >>= \tty -> bool E.evalStdin loop tty ctx st
  where
    loop :: D.Context -> D.Stack -> IO ()
    loop c s = prompt' promptText >>= maybe (T.putStrLn "") process
      where
        process line = if T.null line then loop c s else do
          s' <- E.evalText "(repl)" line c s
          unless (null s' || T.head line `elem` [',',';']) $  --  TODO
            putStrLn $ show $ head s'
          loop c s'

promptText :: Text
promptText = ">>> "

prompt' :: Text -> IO (Maybe Text)
prompt' x = (Just <$> prompt x) `catchIOError`
            \e -> if isEOFError e then return Nothing else ioError e

prompt :: Text -> IO Text
prompt x = do T.putStr x; hFlush stdout; T.getLine

stdinTTY :: IO Bool
stdinTTY = queryTerminal stdInput

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
