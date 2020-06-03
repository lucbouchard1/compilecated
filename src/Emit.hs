{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import qualified Syntax as S

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

convertType :: S.Type -> AST.Type
convertType S.Int = integer
convertType S.Float = double

toSig :: [S.Decl] -> [(AST.Type, AST.Name)]
toSig = map (\(S.Decl t n) -> (convertType t, AST.Name n))

codegenTop :: S.Defn -> LLVM ()
codegenTop (S.Function _ name args body) = do -- TODO: Check return type
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \(S.Decl t n) -> do
        var <- alloca $ convertType t
        store var (local (AST.Name n))
        assign n (convertType t) var
      cgenBody body

-- codegenTop (S.Extern name args) = do
--   external double name fnargs
--   where fnargs = toSig args

codegenTop (S.Extern name args) = return ()

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- NOTE: Since we don't yet have types, functions that return nothing return 0
cgenBody :: [S.Stmt] -> Codegen (AST.Named AST.Terminator)
cgenBody []               = ret $ cons $ C.Float (F.Double 0)
cgenBody (S.Return e :xs) = cgen e >>= ret
cgenBody (x :xs)          = cgenStmt x >> cgenBody xs

cgenStmt :: S.Stmt -> Codegen ()
cgenStmt (S.IfBlk cond t f) = do
  ift <- addBlock "if.true"
  iff <- addBlock "if.false"
  exit <- addBlock "if.exit"

  -- Test condition
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ift iff

  -- Setup true block
  setBlock ift
  forM t cgenStmt
  br exit

  -- Setup false block
  setBlock iff
  forM f cgenStmt
  br exit

  -- Setup exit
  setBlock exit
  return ()
cgenStmt (S.Define (S.Decl t name) e) = do
  i   <- alloca $ convertType t
  val <- cgen e
  store i val
  assign name (convertType t) i
  return ()
cgenStmt (S.Assign name e) = do
  (i, _) <- getvar name
  val <- cgen e
  store i val
  return ()
cgenStmt (S.Return e) = do
  val <- cgen e
  ret val
  return ()

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinaryOp "=" (S.Var var) val) = do
  (a, _) <- getvar var
  cval   <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var x) = do
  (i, _) <- getvar x
  load i
cgen (S.FloatLit n) = return $ cons $ C.Float (F.Double n)
cgen (S.IntLit n)   = return $ cons $ C.Int 32 n
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Defn] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
