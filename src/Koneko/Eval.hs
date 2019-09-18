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

{-# LANGUAGE OverloadedStrings #-}

                                                              --  {{{1
-- |
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Maybe
-- >>> let id = fromJust . ident
--
-- >>> evalCode [str "Hello, World!", KIdent $ id "say"]
-- Hello, World!
--
-- ... TODO ...
--

                                                              --  }}}1

module Koneko.Eval (
  eval, evalList, evalCode, evalFile, evalStdin, evalString, evalText,
  truthy
) where

import Data.Text.Lazy (Text)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data hiding (lookup)

import qualified Koneko.Read as R
import qualified Koneko.Data as D

-- TODO: Either for errors
type Primitive = Scope -> Stack -> IO Stack

-- TODO:
-- * say, +, -, ... --> defaultScope
eval :: KValue -> Scope -> Stack -> IO Stack
eval x sc st = case x of
  KPrim _   -> return (x:st)
  KPair _   -> error "TODO"
  KList _   -> error "TODO"
  KIdent i  -> _evalIdent sc st $ unIdent i
  KQuot _   -> error "TODO"
  KBlock _  -> error "TODO"

-- TODO
_evalIdent :: Scope -> Stack -> Text -> IO Stack
_evalIdent sc st i = case lookup i primitives of
  Just p  -> p sc st
  Nothing -> D.lookup sc i >>= maybe
    (error $ T.unpack i ++ " not found") (primCall sc . (:st))

evalList :: [KValue] -> Scope -> Stack -> IO Stack
evalList []     _  st = return st
evalList (x:xt) sc st = eval x sc st >>= evalList xt sc

evalCode :: [KValue] -> IO ()
evalCode xs = do sc <- newScope; evalList xs sc [] >> return ()

evalFile :: FilePath -> IO ()
evalFile f = do s <- T.readFile f; evalCode $ R.read' f s

evalStdin :: IO ()
evalStdin = T.getContents >>= evalText

evalString :: String -> IO ()
evalString = evalText . T.pack

evalText :: Text -> IO ()
evalText = evalCode . R.read

primitives :: [(Text, Primitive)]
primitives = [                                                --  {{{1
    ("call" , primCall),
    ("def"  , primDef),
    ("if"   , primIf),
    ("say"  , primSay)
  ]                                                           --  }}}1

primCall, primDef, primIf, primSay :: Primitive

primCall _ (_:_ ) = error "TODO"                              --  TODO
primCall _ _      = stackUnderflow

primDef __ (_:_:_ ) = error "TODO"                            --  TODO
primDef _  _        = stackUnderflow

primIf sc (fb:tb:b:st)  = primCall sc $ if truthy b then tb:st else fb:st
primIf _  _             = stackUnderflow

primSay _  (KPrim (KStr s):st)  = st <$ T.putStrLn s
primSay _  _                    = matchError "say"

truthy :: KValue -> Bool
truthy (KPrim KNil)           = False
truthy (KPrim (KBool False))  = False
truthy _                      = True

stackUnderflow :: a
stackUnderflow = error "*** stack underflow ***"

matchError :: String -> a
matchError s = error $ "*** match failed for " ++ s ++ " ***"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
