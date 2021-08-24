{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Repl where

import           AST
import           Eval
import           Pretty
import           Parser

import           Control.Applicative        (Alternative ((<|>)), optional)
import           Control.Monad
import           Control.Monad.State
import           Control.Exception
import           System.IO

import           Text.Printf
import           Data.HashMap.Strict        as H
import           Data.Text                  (Text)
import           Data.Void
import qualified Data.Text                  as T
import           Text.Megaparsec            (ParseErrorBundle, parse, errorBundlePretty)


handleCommand :: String -> [String] -> StateT Context IO ()
handleCommand "load" arg = acc arg
  where
    acc :: [String] -> StateT Context IO ()
    acc [] = pure ()
    acc arg = do c <- get
                 l <- (lift . loadContext . head) arg
                 put $ H.union c l
                 acc $ tail arg
handleCommand "context" [] = do c <- get
                                mapM_ (lift . (\t -> printf "%5s = %s\n" (fst t) ((pretty . snd) t))) (H.toList c)
handleCommand "context" arg = do c <- get
                                 case c !? head arg of
                                   Just a -> lift $ printf "%5s = %s\n" (head arg) (pretty a)
                                   Nothing -> lift . putStrLn $ head arg ++ " does not exist in context"
handleCommand n _ = lift . putStrLn $ "Command not found: " ++ n

run :: [String] -> Context -> Either (ParseErrorBundle Text Void) Context
run [] c = Right c
run ls c = case parse pExpr "" . T.pack $ head ls of
              Right a -> case eval (c, a) of
                             (nc, _) -> run (tail ls) nc
              Left e -> Left e

loadContext :: FilePath -> IO Context
loadContext f = do res <- try load :: IO (Either SomeException Context)
                   case res of
                     Right a -> return a
                     Left e -> do putStrLn $ "Failed : \n\t" ++ show e
                                  return H.empty
  where 
    load = do file <- openFile f ReadMode
              lines <- (fmap lines . hGetContents) file
              case run lines H.empty of
                Left e -> do (putStr . errorBundlePretty) e
                             return H.empty
                Right c -> return c


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
