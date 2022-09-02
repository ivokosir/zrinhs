module Parser (Expression, Extra (..), parse) where

import AST
import Error
import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P

data Extra = Extra {extraStart :: SourcePos, extraEnd :: SourcePos} deriving (Eq, Show)

type Expression = ExpressionGeneric Extra

type Base = ExpressionBase Expression

languageDef =
  emptyDef
    { P.commentStart = "/*",
      P.commentEnd = "*/",
      P.commentLine = "//",
      P.reservedOpNames =
        [ "*",
          "/",
          "%",
          "+",
          "-",
          ">",
          ">=",
          "<",
          "<=",
          "==",
          "!=",
          "and",
          "or",
          "->"
        ],
      P.reservedNames =
        [ "and",
          "or",
          "true",
          "false",
          "if",
          "then",
          "else",
          "end"
        ]
    }

lexer = P.makeTokenParser languageDef

identifier = P.identifier lexer

reserved = P.reserved lexer

reservedOp = P.reservedOp lexer

parens = P.parens lexer

natural = P.natural lexer

semiSep = P.semiSep lexer

commaSep = P.commaSep1 lexer

whiteSpace = P.whiteSpace lexer

stringLiteral = P.stringLiteral lexer

withPosition :: Parser Base -> Parser Expression
withPosition p = do
  start <- getPosition
  ast <- p
  end <- getPosition
  return $ Expression ast (Extra start end)

formatBlockBody :: [Expression] -> Base
formatBlockBody [] = Literal CUnit
formatBlockBody es = Block (init es) (last es)

parser :: Parser Expression
parser = do
  whiteSpace
  ast <- withPosition (formatBlockBody <$> blockBody)
  eof
  return ast

block :: Parser Expression
block = withPosition (formatBlockBody <$> parens blockBody)

blockBody :: Parser [Expression]
blockBody = semiSep expression

expression :: Parser Expression
expression = try definition <|> try tuple <|> try function <|> ifThenElse <|> operators

definition :: Parser Expression
definition = withPosition $ do
  name <- identifier
  reservedOp "="
  Definition name <$> expression

tuple :: Parser Expression
tuple = withPosition (Tuple <$> parens (commaSep expression))

function :: Parser Expression
function = withPosition $ do
  param <- identifier
  reservedOp "->"
  Function param <$> expression

ifThenElse :: Parser Expression
ifThenElse = withPosition $ do
  reserved "if"
  condition <- expression
  reserved "then"
  thenExpression <- expression
  reserved "else"
  elseExpression <- expression
  reserved "end"
  return $ IfThenElse condition thenExpression elseExpression

operators :: Parser Expression
operators = buildExpressionParser operatorsTable call

operatorsTable =
  let toOperaton operation lhs rhs =
        Expression
          (Operation operation lhs rhs)
          (Extra (extraStart (extra lhs)) (extraEnd (extra rhs)))
      binary name operation =
        Infix (reservedOp name >> return (toOperaton operation)) AssocLeft
   in [ [binary "*" Multiply, binary "/" Divide, binary "%" Remainder],
        [binary "+" Add, binary "-" Subtract],
        [ binary ">" Greater,
          binary ">=" GreaterEqual,
          binary "<" Less,
          binary "<=" LessEqual
        ],
        [binary "==" Equal, binary "!=" NotEqual],
        [binary "and" And],
        [binary "or" Or]
      ]

call :: Parser Expression
call = do
  caller <- primary
  ( do
      callee <- call
      return
        ( Expression
            (Call caller callee)
            (Extra (extraStart (extra caller)) (extraEnd (extra callee)))
        )
    )
    <|> return caller

primary :: Parser Expression
primary =
  let primary_ =
        (reserved "true" >> return (Literal (CBool True)))
          <|> (reserved "false" >> return (Literal (CBool False)))
          <|> Literal
            . CInt
            . fromInteger
          <$> natural
          <|> Literal
            . CString
          <$> stringLiteral
          <|> Identifier
          <$> identifier
   in withPosition primary_ <|> block

parse :: SourceName -> String -> Either [Error] Expression
parse name source = case P.parse parser name source of
  Right e -> Right e
  Left error -> Left [ParseError error]
