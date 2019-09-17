--  --                                                          ; {{{1
--
--  File        : Koneko/Eval.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-17
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

module Koneko.Eval (
  eval, evalFile, evalStdin, evalString, evalText
) where

import Control.Monad ((>=>))
import Data.Text.Lazy (Text)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Read as R

-- TODO
eval code = error "eval not yet implemented"

evalFile :: FilePath -> IO ()
evalFile = T.readFile >=> evalText

evalStdin :: IO ()
evalStdin = T.getContents >>= evalText

evalString :: String -> IO ()
evalString = evalText . T.pack

evalText :: Text -> IO ()
evalText = eval . R.read

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
