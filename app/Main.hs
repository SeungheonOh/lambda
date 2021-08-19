{-# LANGUAGE OverloadedStrings #-}
module Main where

import AST
import Pretty

import System.IO
import Control.Applicative hiding (many)
import Control.Monad
import Data.Char

import           Data.Text (Text)
import qualified Data.Text            as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

pName :: Parser Name
pName = (:[]) <$> (satisfy isAlphaNum :: Parser Char)

-- Variable is a single character
pVar :: Parser Expr 
pVar = Var <$> pName

-- λv.e \v.e Both works for Lambda
pLam :: Parser Expr
pLam = (char 'λ' <|> char '\\') *> pName <* char '.'
        >>= (\n -> Lam n <$> pExpr)

pParen :: Parser a -> Parser a
pParen p = char '(' *> p <* char ')'

-- Every Lambda and Variable is a Term
pTerm :: Parser Expr
pTerm = try (pParen pExpr)
      <|> try pLam
      <|> pVar

-- Constructs Application based on all Terms
pExpr :: Parser Expr
pExpr = pApp (space *> pTerm <* space)

pApp :: Parser Expr -> Parser Expr
pApp p = p >>= go
  where
    go acc = do
      r <- optional p
      case r of
        Nothing -> return acc
        Just x -> go (App acc x)

main :: IO ()
main = forever $ hSetBuffering stdin LineBuffering
                 >> putStr ">"
                 >> getLine 
                 >>= \s -> either (putStr . errorBundlePretty) 
                                  (\a -> do putStr "Parsed : "
                                            putStrLn $ pretty a 
                                            putStr "AST    : "
                                            print a) 
                                  (parse pExpr "" . T.pack $ s)
                                  