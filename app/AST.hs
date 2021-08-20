module AST where 
  
type Name = String

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          deriving (Show, Eq)