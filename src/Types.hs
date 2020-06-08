{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad.State
import Control.Applicative
import Control.Monad.Except

import qualified Syntax as S

type SymbolTable = [(String, S.Type)]

data TypeCheckState 
  = TypeCheckState {
    symtab :: SymbolTable
  }
  deriving (Show)

newtype TypeCheck a = TypeCheck { runTypeCheck :: State TypeCheckState a}
  deriving (Functor, Applicative, Monad, MonadState TypeCheckState )

-- Type Check Operations

emptyTypeCheck :: TypeCheckState
emptyTypeCheck = TypeCheckState []

execTypeCheck :: TypeCheck a -> TypeCheckState
execTypeCheck m = execState (runTypeCheck m) emptyTypeCheck

-- Symbol Table

assign :: String -> S.Type -> TypeCheck ()
assign var x = do
  syms <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ syms }

getvar :: String -> TypeCheck S.Type
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-- Check types

checkType :: S.Expr -> TypeCheck S.Type
checkType (S.Var s)          = getvar s
checkType (S.Call _ _)      = return S.Int -- TODO: Return the actual function type
checkType (S.BinaryOp _ l r) = do -- TODO: Differentiate on different operations
  lt <- checkType l
  rt <- checkType r
  if lt == rt then return lt else error $ "Expected type: " ++ show lt
checkType (S.UnaryOp _ e) = checkType e
checkType (S.FloatLit _)  = return S.Float
checkType (S.IntLit _)    = return S.Int

checkDecl :: S.Decl -> TypeCheck ()
checkDecl (S.Decl t n) = assign n t

checkStmt :: S.Stmt -> TypeCheck ()
checkStmt (S.IfBlk c _ _) = do
  t <- checkType c
  if t == S.Int || t == S.Float then return () else error "Expected integral type"
checkStmt (S.Define (S.Decl t n) e) = do
  expT <- checkType e
  if expT == t then assign n t else error $ "Expected type: " ++ show t
  return ()
checkStmt (S.Assign n e) = do
  varT <- getvar n
  expT <- checkType e
  if varT == expT then return () else error $ "Expected type: " ++ show varT
checkStmt (S.Return e) = do
  checkType e
  return ()

checkTopLevel :: S.Defn -> TypeCheck ()
checkTopLevel (S.Function t n args body) = do
  forM args checkDecl
  forM body checkStmt
  return ()
checkTopLevel (S.Extern _ _) = return ()

typecheck :: [S.Defn] -> IO ()
typecheck ast = do
  v <- return $ map execTypeCheck (map checkTopLevel ast)
  putStrLn $ show v
  return ()