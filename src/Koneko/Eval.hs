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
-- >>> ev [str "Hello, World!", KIdent $ id "__say__"]
-- Hello, World!
--
-- >>> evalList [val 1, val 2, KIdent $ id "__int_-__"] undefined []
-- [-1]
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
eval x c s = case x of
  KPrim _   -> return $ s `push` x
  KPair _   -> error "TODO"
  KList _   -> error "TODO"
  KIdent i  -> _evalIdent (unIdent i) c s
  KQuot _   -> error "TODO"
  KBlock _  -> error "TODO"

-- TODO
_evalIdent :: Text -> Evaluator
_evalIdent i c s = case lookup i primitives of
    Just p  -> p c s
    Nothing -> D.lookup c i >>= maybe err (primCall c . push s)
  where
    err = error $ "*** lookup failed: " ++ T.unpack i ++ " ***"

evalList :: [KValue] -> Evaluator
evalList []     _ s = return s
evalList (x:xt) c s = eval x c s >>= evalList xt c

evalText :: FilePath -> Text -> Evaluator
evalText name code c s = evalList (R.read' name code) c s

evalStdin :: Context -> Stack -> IO ()
evalStdin c s = () <$ do
  code <- T.getContents; evalText "(stdin)" code c s

-- primitives --

-- TODO
primitives :: [(Text, Evaluator)]
primitives = [                                                --  {{{1
    ("__call__"   , primCall),
    ("__def__"    , primDef),
    ("__if__"     , primIf),
    ("__say__"    , primSay),
    ("__int_+__"  , primIntArith (+)),
    ("__int_-__"  , primIntArith (-)),
    ("__int_*__"  , primIntArith (*))
  ]                                                           --  }}}1

primCall, primDef, primIf, primSay :: Evaluator

primCall  = error "TODO"
primDef   = error "TODO"

primIf c s  = let ((b, tb, fb), s') = pop' s
              in primCall c $ push' s' $ if truthy b then tb else fb

primSay _ s = let (x, s') = pop' s in s' <$ T.putStrLn x

primIntArith :: (Integer -> Integer -> Integer) -> Evaluator
primIntArith op _ s
  = let ((x,y),s') = pop' s in return $ s' `push` (x `op` y)

-- utilities --

truthy :: KValue -> Bool
truthy (KPrim KNil)           = False
truthy (KPrim (KBool False))  = False
truthy _                      = True

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
