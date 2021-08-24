{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Parser where

import           AST

import           Data.Char
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

pName :: Parser Name
pName = (:[]) <$> (satisfy isAlphaNum :: Parser Char)
     <|> string "\"" *> many (anySingleBut '\"') <* string "\""
     <|> char '#' *> many (anySingleBut ' ') <* space

-- Variable is a single character
pVar :: Parser UTLC
pVar = Var <$> pName

-- λv.e \v.e Both works for Lambda
pLam :: Parser UTLC
pLam = (char 'λ' <|> char '\\') *> pName <* char '.'
        >>= (\n -> Lam n <$> pUTLC)

pParen :: Parser a -> Parser a
pParen p = char '(' *> p <* char ')'

-- Every Lambda and Variable is a Term
pTerm :: Parser UTLC
pTerm = try (pParen pUTLC)
     <|>try pLam
     <|>pVar

-- Constructs Application based on all Terms
pUTLC :: Parser UTLC
pUTLC = pApp (space *> pTerm <* space)

pApp :: Parser UTLC -> Parser UTLC
pApp p = p >>= go
  where
    go acc = do
      r <- optional p
      case r of
        Nothing -> return acc
        Just x  -> go (App acc x)

pExpr :: Parser Expr
pExpr = try (Assign <$> pName <* (space *> string ":=" <* space) <*> pUTLC)
     <|>Command <$> (char '!' *> many alphaNumChar) <*> many (space *> some (anySingleBut ' '))
     <|>Calc <$> pUTLC
     