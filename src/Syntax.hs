module Syntax where

type Name = String

data Type
  = Void
  | Float
  | Int
  deriving (Eq, Ord, Show)

data Expr
  = FloatLit Double
  | IntLit Integer
  | Var String
  | Call Name [Expr]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  deriving (Eq, Ord, Show)

data Decl = Decl Type String
  deriving (Eq, Ord, Show)

data Stmt
  = IfBlk Expr [Stmt] [Stmt]
  | Define Decl Expr
  | Assign String Expr
  | Return Expr
  deriving (Eq, Ord, Show)

data Defn
  = Function Type Name [Decl] [Stmt]
  | Extern Name [Name]
  deriving (Eq, Ord, Show)
