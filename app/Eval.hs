module Eval where

import AST
import Data.HashMap.Strict

newtype Environment = Env (HashMap Name Expr , Expr) 

isNF :: Expr -> Bool
isNF (Var _) = True 
isNF (Lam _ e) = isNF e
isNF (App (Lam _ _) e) = False
isNF (App e1 e2) = isNF e1 && isNF e2