{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           AST
import           Eval
import           Pretty

import           Control.Applicative        (Alternative ((<|>)), optional)
import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           System.IO

import           Data.HashMap.Strict        as H
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
     
repl :: StateT Context IO ()
repl = do lift $ putStr ">"
          l <- lift getLine
          case parse pExpr "" . T.pack $ l of
            Left e -> (lift . putStr . errorBundlePretty) e
            Right a ->
              do context <- get
                 case a of
                   Command c a -> handleCommand c a
                   _ -> case eval (context, a) of
                         (c, Just e) -> do put c
                                           lift $ putStrLn $ pretty e
                         (c,Nothing) -> do put c
                                           lift $ putStrLn "OK"
          repl
          
handleCommand :: String -> [String] -> StateT Context IO ()
handleCommand "load" arg = (lift . loadContext . head) arg >>= put
handleCommand n _ = lift . putStrLn $ "Command not found: " ++ n

run :: [String] -> Context -> Either (ParseErrorBundle Text Void) Context
run [] c = Right c
run ls c = case parse pExpr "" . T.pack $ head ls of
              Right a -> case eval (c, a) of
                             (nc, _) -> run (tail ls) nc
              Left e -> Left e

loadContext :: FilePath -> IO Context
loadContext f = do lines <- (fmap lines . readFile) f
                   case run lines H.empty of
                     Left e -> do (putStr . errorBundlePretty) e
                                  return H.empty
                     Right c -> return c

main :: IO ()
main = do hSetBuffering stdin LineBuffering
          execStateT repl H.empty 
          putStrLn "DONE"