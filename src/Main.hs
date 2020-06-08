module Main where

import Parser
import Emit
import Codegen
import Types

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.AST as AST

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      return $ typecheck ex
      ast <- codegen modo ex
      return $ Just ast

printAst :: String -> IO ()
printAst source = do
  let res = parseToplevel source
  case res of
    Left err -> print err
    Right ex -> print ex

processAst :: String -> IO ()
processAst fname = readFile fname >>= printAst

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> return ()
    [fname] -> processFile fname >> return ()
