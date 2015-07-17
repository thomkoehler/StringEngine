----------------------------------------------------------------------------------------------------

module Text.StringEngine.Parser
(
   StrExpr(..),
   parseStr
)
where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

import Control.Monad.Identity

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
            "foreach",
            "end",
            "in"
         ],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = ["++"],
      P.caseSensitive  = True
   }


data StrExpr
   = ExprStrLit String
   | ExprVar String
   | ExprForeach String String [StrExpr]
   deriving(Show, Eq)


lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser languageDef


identifier :: Parser String
identifier = P.identifier lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer


strExpr :: Parser StrExpr
strExpr = choice
   [
      strLit,
      var,
      foreach
   ]


strLit :: Parser StrExpr
strLit = liftM ExprStrLit stringLiteral


var :: Parser StrExpr
var = liftM ExprVar identifier


foreach :: Parser StrExpr
foreach = do
   reserved "foreach"
   sel <- identifier
   reserved "in"
   list <- identifier
   exprs <- many strExpr
   reserved "end"
   return $ ExprForeach sel list exprs


parseStr :: String -> [StrExpr]
parseStr str = case parse (many1 strExpr) "" str of
   Left err -> error $ show err
   Right xs -> xs

----------------------------------------------------------------------------------------------------
