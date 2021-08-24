{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import Repl
import Eval

import System.IO
import Control.Monad.State

main :: IO ()
main = do hSetBuffering stdin LineBuffering
          execStateT repl emptyContext
          putStrLn "DONE"