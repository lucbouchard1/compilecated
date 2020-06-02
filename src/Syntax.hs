module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  deriving (Eq, Ord, Show)

data Stmt
  = IfBlk Expr [Stmt] [Stmt]
  | Decl String
  | Assign String Expr
  | Return Expr
  deriving (Eq, Ord, Show)

data Defn
  = Function Name [Name] [Stmt]
  | Extern Name [Name]
  deriving (Eq, Ord, Show)
