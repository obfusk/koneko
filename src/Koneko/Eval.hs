--  --                                                          ; {{{1
--
--  File        : Koneko/Eval.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-18
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

                                                              --  {{{1
-- |
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Maybe
-- >>> let id = fromJust . ident
-- >>> let ev x = () <$ evalList x undefined []
--
-- >>> ev [str "Hello, World!", KIdent $ id "say"]
-- Hello, World!
--
-- ... TODO ...
--

                                                              --  }}}1

module Koneko.Eval (eval, evalList, evalText, evalStdin, truthy) where

import Data.Text.Lazy (Text)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data hiding (lookup)

import qualified Koneko.Read as R
import qualified Koneko.Data as D

-- TODO: Either for errors
type Evaluator = Context -> Stack -> IO Stack

-- TODO:
-- * say, +, -, ... --> defaultScope
eval :: KValue -> Evaluator
eval x sc st = case x of
  KPrim _   -> return (x:st)
  KPair _   -> error "TODO"
  KList _   -> error "TODO"
  KIdent i  -> _evalIdent (unIdent i) sc st
  KQuot _   -> error "TODO"
  KBlock _  -> error "TODO"

-- TODO
_evalIdent :: Text -> Evaluator
_evalIdent i sc st = case lookup i primitives of
  Just p  -> p sc st
  Nothing -> D.lookup sc i >>= maybe
    (error $ T.unpack i ++ " not found") (primCall sc . (:st))

evalList :: [KValue] -> Evaluator
evalList []     _  st = return st
evalList (x:xt) sc st = eval x sc st >>= evalList xt sc

evalText :: FilePath -> Text -> Evaluator
evalText name code c s = evalList (R.read' name code) c s

evalStdin :: Context -> Stack -> IO ()
evalStdin c s = () <$ do
  code <- T.getContents; evalText "(stdin)" code c s

-- primitives --

primitives :: [(Text, Evaluator)]
primitives = [                                                --  {{{1
    ("call" , primCall),
    ("def"  , primDef),
    ("if"   , primIf),
    ("say"  , primSay)
  ]                                                           --  }}}1

primCall, primDef, primIf, primSay :: Evaluator

primCall _ (_:_ ) = error "TODO"                              --  TODO
primCall _ _      = stackUnderflow

primDef __ (_:_:_ ) = error "TODO"                            --  TODO
primDef _  _        = stackUnderflow

primIf sc (fb:tb:b:st)  = primCall sc $ if truthy b then tb:st else fb:st
primIf _  _             = stackUnderflow

primSay _  (KPrim (KStr s):st)  = st <$ T.putStrLn s
primSay _  _                    = matchError "say"

-- utilities --

truthy :: KValue -> Bool
truthy (KPrim KNil)           = False
truthy (KPrim (KBool False))  = False
truthy _                      = True

stackUnderflow :: a
stackUnderflow = error "*** stack underflow ***"

matchError :: String -> a
matchError s = error $ "*** match failed for " ++ s ++ " ***"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
