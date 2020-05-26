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
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

binops = [[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]]

expr :: Parser Expr
expr =  Ex.buildExpressionParser binops factor

stmt :: Parser Stmt
stmt =  do
  e <- expr
  reservedOp ";"
  return e

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Defn
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  body <- braces $ many stmt
  return $ Function name args body

extern :: Parser Defn
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

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
