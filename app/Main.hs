{-# LANGUAGE OverloadedStrings #-}
module Main where

import AST
import Pretty
import Eval

import System.IO
import Control.Monad.State
import Control.Applicative ( Alternative((<|>)), optional )
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
     <|> string "\"" *> many alphaNumChar <* string "\""

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
        Just x -> go (App acc x)
        
pExpr :: Parser Expr 
pExpr = try $ Assign <$> pName <* (space *> string ":=" <* space) <*> pUTLC
     <|>Calc <$> pUTLC

runString :: String -> IO ()
runString s = either 
              (putStr . errorBundlePretty)
              (\a -> do print a)
              -- ((\a -> do putStr "Parsed : "
              --            putStrLn $ pretty a
              --            putStr "AST    : "
              --            print a
              --            putStr "IsNF   : "
              --            print $ isNF a
              --            putStr "isHNF  : "
              --            print $ isHNF a) . reduction)
              (parse pExpr "" . T.pack $ s)
              
main :: IO ()
main = forever $ hSetBuffering stdin LineBuffering
                 >> putStr ">"
                 >> getLine
                 >>= \s -> runString s
                                  