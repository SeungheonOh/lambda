{-# LANGUAGE OverloadedStrings #-}
module Eval where

import AST
import Data.HashMap.Strict

type Environment = (HashMap Name Expr , Expr) 

isNF :: Expr -> Bool
isNF (Var _) = True 
isNF (Lam _ e) = isNF e
isNF (App (Lam _ _) e) = False
isNF (App e1 e2) = isNF e1 && isNF e2

isHNF :: Expr -> Bool
isHNF (Var _) = True 
isHNF (Lam _ e) = isNF e
isHNF (App (Lam _ _) e) = False
isHNF (App e1 e2) = isNF e1

substitute :: Expr -- Expression
  -> Expr -- Expression to replace
  -> Expr -- New expression
  -> Expr
substitute (Var e) o n = if Var e == o then n 
                                       else Var e
substitute (App a b) o n = App (substitute a o n) (substitute b o n)
substitute (Lam a f) o n = Lam a (substitute f o n)

reduction :: Expr -> Expr
reduction (App (Lam n e1) e2) = substitute e1 (Var n) e2
reduction e = e 

eval :: Environment -> Environment
eval (m, e) = (m, e)

