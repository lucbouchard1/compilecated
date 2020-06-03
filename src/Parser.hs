module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = do
  n <- integer
  return $ IntLit (fromInteger n)

floating :: Parser Expr
floating = FloatLit <$> float

binary s = Ex.Infix (reservedOp s >> return (BinaryOp s))
prefix s = Ex.Prefix (reservedOp s >> return (UnaryOp s))

ops = [ [binary "*" Ex.AssocLeft, binary "/" Ex.AssocLeft]
      , [binary "+" Ex.AssocLeft, binary "-" Ex.AssocLeft]
      ]

expr :: Parser Expr
expr =  Ex.buildExpressionParser ops factor

variable :: Parser Expr
variable = Var <$> identifier

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> (parens expr)

intType :: Parser Type
intType = do
  reserved "int"
  return Int

floatType :: Parser Type
floatType = do
  reserved "float"
  return Float

voidType :: Parser Type
voidType = do
  reserved "void"
  return Void

factorType :: Parser Type
factorType = intType <|> floatType <|> voidType

decl :: Parser Decl
decl = do
  t    <- factorType
  name <- identifier
  return $ Decl t name

ifblk :: Parser Stmt
ifblk = do
  reserved "if"
  cond <- parens $ expr
  t    <- braces $ many stmt
  reserved "else"
  f    <- braces $ many stmt
  return $ IfBlk cond t f

define :: Parser Stmt
define = do
  d    <- decl
  reservedOp "="
  val  <- expr
  reservedOp ";"
  return $ Define d val

assign :: Parser Stmt
assign = do
  name <- identifier
  reservedOp "="
  val  <- expr
  reservedOp ";"
  return $ Assign name val

ret :: Parser Stmt
ret = do
  reservedOp "return"
  val  <- expr
  reservedOp ";"
  return $ Return val

stmt :: Parser Stmt
stmt = try define
  <|> try assign
  <|> ret
  <|> ifblk

function :: Parser Defn
function = do
  t    <- factorType
  name <- identifier
  args <- parens $ commaSep decl
  body <- braces $ many stmt
  return $ Function t name args body

extern :: Parser Defn
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

defn :: Parser Defn
defn = try extern
    <|> try function

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Defn]
toplevel = many $ do
    def <- defn
    return def

-- parseExpr :: String -> Either ParseError Expr
-- parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Defn]
parseToplevel s = parse (contents toplevel) "<stdin>" s
