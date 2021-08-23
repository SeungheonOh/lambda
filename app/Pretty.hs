{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import AST

import Prelude(String, Int)
import Text.PrettyPrint

-- pretty' :: UTLC -> Doc
-- pretty' (Lam a b) = text "λ" <> text a <> char '.' <> pretty' b
-- pretty' (App a b) = char '(' <> pretty' a <> char ' ' <> pretty' b <> char ')'
-- pretty' (Var a) = text a

-- pretty :: UTLC -> String 
-- pretty e = render (pretty' e)


pretty' :: Int -> UTLC -> Doc
pretty' 0 (Lam a b) = text "λ" <> text a <> char '.' <> pretty' 0 b
pretty' 0 a = pretty' 1 a
pretty' 1 (App a b) = pretty' 1 a <> char ' ' <> pretty' 2 b
pretty' 1 a = pretty' 2 a
pretty' 2 (Var a) = text a
pretty' 2 a = char '(' <> pretty' 0 a <> char ')'

pretty :: UTLC -> String 
pretty e = render (pretty' 0 e)