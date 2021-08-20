{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import AST

import Prelude(String)
import Text.PrettyPrint

pretty' :: UTLC -> Doc
pretty' (Lam a b) = text "λ" <> text a <> char '.' <> pretty' b
pretty' (App a b) = char '(' <> pretty' a <> char ' ' <> pretty' b <> char ')'
pretty' (Var a) = text a

pretty :: UTLC -> String 
pretty e = render (pretty' e)