module AST where 
  
type Name = String

data UTLC = Var Name
          | App UTLC UTLC
          | Lam Name UTLC
          deriving (Show, Eq)