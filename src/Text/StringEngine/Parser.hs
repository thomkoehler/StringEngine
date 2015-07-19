----------------------------------------------------------------------------------------------------

module Text.StringEngine.Parser
(
   Expression(..),
   parseStr
)
where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Control.Monad.Identity

import Text.StringEngine.DynAny

----------------------------------------------------------------------------------------------------

type Parser a = ParsecT String () Identity a

languageDef :: GenLanguageDef String () Identity
languageDef = P.LanguageDef
   {
      P.commentStart = "/*",
      P.commentEnd = "*/",
      P.commentLine  = "//",
      P.nestedComments = True,
      P.identStart  = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.reservedNames =
         [
            "for",
            "end",
            "in",
            "True",
            "False",
            "if"
         ],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = ["++"],
      P.caseSensitive  = True
   }


data Expression
   = ExprLiteral DynAny
   | ExprVar String
   | ExprForeach String String [Expression]
   | ExprIf Expression [Expression]
   deriving Eq


lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser languageDef


identifier :: Parser String
identifier = P.identifier lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer


expression :: Parser Expression
expression = choice
   [
      strLit,
      boolLit,
      var,
      foreach,
      exprIf
   ]


strLit :: Parser Expression
strLit = liftM (ExprLiteral . DynString) stringLiteral


var :: Parser Expression
var = liftM ExprVar identifier


foreach :: Parser Expression
foreach = do
   reserved "for"
   sel <- identifier
   reserved "in"
   list <- identifier
   exprs <- many expression
   reserved "end"
   return $ ExprForeach sel list exprs


boolLit :: Parser Expression
boolLit = choice
   [
      reserved "True" >> return (ExprLiteral dynTrue),
      reserved "False" >> return (ExprLiteral dynFalse)
   ]


exprIf :: Parser Expression
exprIf = do
   reserved "if"
   b <- expression
   exprs <- many expression
   reserved "end"
   return $ ExprIf b exprs


parseStr :: String -> [Expression]
parseStr str = case parse (many1 expression) "" str of
   Left err -> error $ show err
   Right xs -> xs

----------------------------------------------------------------------------------------------------
