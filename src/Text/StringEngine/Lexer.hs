
----------------------------------------------------------------------------------------------------

module Text.StringEngine.Lexer
(
   Parser,
   identifier,
   stringLiteral,
   reserved
)
where

import Control.Monad.Identity
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec

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

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser languageDef


identifier :: Parser String
identifier = P.identifier lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

----------------------------------------------------------------------------------------------------
