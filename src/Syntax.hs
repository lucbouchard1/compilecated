module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  deriving (Eq, Ord, Show)

-- Statements end with a semicolon
type Stmt
  = Expr

data Defn
  = Function Name [Name] [Stmt]
  | Extern Name [Name]
  deriving (Eq, Ord, Show)
