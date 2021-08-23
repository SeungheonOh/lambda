{-# LANGUAGE OverloadedStrings #-}
module Eval where

import AST ( Expr(..), UTLC(..), Name )
import Data.HashMap.Strict

type Context = HashMap Name UTLC
type Environment = (Context, Expr)

isNF :: UTLC -> Bool
isNF (Var _) = True
isNF (Lam _ e) = isNF e
isNF (App (Lam _ _) e) = False
isNF (App e1 e2) = isNF e1 && isNF e2

isHNF :: UTLC -> Bool
isHNF (Var _) = True
isHNF (Lam _ e) = isNF e
isHNF (App (Lam _ _) e) = False
isHNF (App e1 e2) = isNF e1

substitute :: UTLC -- Expression
           -> UTLC -- Expression to replace
           -> UTLC -- New expression
           -> UTLC
substitute (Var e) o n = if Var e == o then n
                                       else Var e
substitute (App a b) o n = App (substitute a o n) (substitute b o n)
substitute (Lam a f) o n = Lam a (substitute f o n)

stepReduction :: UTLC -> UTLC -- single step reduction
stepReduction (App (Lam n e1) e2) = substitute (stepReduction e1) (Var n) (stepReduction e2)
stepReduction (App e1 e2) = App (stepReduction e1) (stepReduction e2)
stepReduction (Lam n e1) = Lam n (stepReduction e1)
stepReduction e = e

reduction :: UTLC -> UTLC
reduction e
  | not $ isNF e = reduction $ stepReduction e
  | otherwise = e
  
hasKey :: Context -> UTLC -> Bool
hasKey c (Var a) = member a c
hasKey c (App e1 e2) = hasKey c e1 || hasKey c e2
hasKey c (Lam _ e1) = hasKey c e1
 
apply :: Context -> UTLC -> UTLC
apply c (Var a) = case c !? a of
                    Just v -> v 
                    Nothing -> Var a
apply c (App e1 e2) = App (apply c e1) (apply c e2)
apply c (Lam n e1) = Lam n (apply c e1)

applyAndReduce :: Context -> UTLC -> UTLC
applyAndReduce c e 
  | hasKey c e = applyAndReduce c $ (reduction . apply c) e
  | otherwise = e

eval :: Environment -> (Context, Maybe UTLC)
eval (m, Assign n e) = (insert n e m, Nothing)
eval (m, Calc e) = (m, Just $ applyAndReduce m e)
eval (m, Command e a) = (m, Nothing) -- running command requires IO