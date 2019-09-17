{-# LANGUAGE OverloadedStrings #-}

module Koneko.Repl (repl) where

import Control.Monad (unless)
import Data.Text (Text)
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, isEOFError)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- TODO: readline? or just rlwrap?

repl :: IO ()
repl = do
    isatty <- queryTerminal stdInput
    if isatty then loop else T.interact (T.unlines . map f . T.lines)
  where
    loop      = prompt' ">>> " >>=
                maybe (T.putStrLn "") (\l -> process l >> loop)
    process l = unless (T.null l) (T.putStrLn $ f l)

-- TODO
f :: Text -> Text
f = T.reverse

prompt' :: Text -> IO (Maybe Text)
prompt' x = (Just <$> prompt x) `catchIOError`
            \e -> if isEOFError e then return Nothing else ioError e

prompt :: Text -> IO Text
prompt x = do T.putStr x; hFlush stdout; T.getLine
