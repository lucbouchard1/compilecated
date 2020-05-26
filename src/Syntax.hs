module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  deriving (Eq, Ord, Show)

data Defn
  = Function Name [Name] [Expr]
  | Extern Name [Name]
  deriving (Eq, Ord, Show)
