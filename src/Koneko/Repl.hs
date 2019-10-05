--  --                                                          ; {{{1
--
--  File        : Koneko/Repl.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-04
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Repl (
  repl, repl', promptText, errorText, stdinTTY
) where

import Control.DeepSeq ((<$!!>))
import Control.Monad (unless)
import Data.String (IsString)
import Data.Text.Lazy (Text)
import System.IO (hFlush, hPutStrLn, stdout, stderr)
import System.IO.Error (catchIOError, isEOFError)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import qualified Koneko.Data as D
import qualified Koneko.Eval as E

import qualified Koneko.Prim as Prim

-- TODO: readline? or just rlwrap?

repl :: D.Context -> D.Stack -> IO ()
repl c s = () <$ repl' False promptText c s

-- | NB: when an exception is caught during the evaluation of a line,
-- the exeption is printed and the repl continues with the stack reset
-- to what it was before that line; however, any definitions that were
-- added to a module before the exception occurred will have taken
-- effect.
repl' :: Bool -> Text -> D.Context -> D.Stack -> IO D.Stack
repl' breakOnError pr ctx st = Prim.replDef ctx >> loop ctx st
  where
    loop :: D.Context -> D.Stack -> IO D.Stack
    loop c s = prompt' pr >>= maybe (s <$ T.putStrLn "") process
      where
        process line = if T.null line then loop c s else do
          r <- E.tryK $ id <$!!> E.evalText "(repl)" line c s
          case r of
            Left e    -> do hPutStrLn stderr $ errorText ++ show e
                            if breakOnError then return s else loop c s
            Right s'  -> do unless (shouldSkip s' line) $
                              putStrLn $ show $ head s'       --  TODO
                            loop c s'

shouldSkip :: D.Stack -> Text -> Bool
shouldSkip s line = null s || T.head line `elem` [',',';']

promptText, errorText :: IsString s => s
promptText  = ">>> "
errorText   = "*** ERROR: "

stdinTTY :: IO Bool
stdinTTY = queryTerminal stdInput

-- utilities --

prompt' :: Text -> IO (Maybe Text)
prompt' x = (Just <$> prompt x) `catchIOError` \e ->
            if isEOFError e then return Nothing else ioError e

prompt :: Text -> IO Text
prompt x = do T.putStr x; hFlush stdout; T.getLine

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
