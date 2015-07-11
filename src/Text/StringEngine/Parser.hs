----------------------------------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module Text.StringEngine.Parser
(
   StringExpr(..)
)
where

import Text.Parsec hiding(State, parse)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

import Control.Monad.Identity

----------------------------------------------------------------------------------------------------

languageDef :: GenLanguageDef String st Identity
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
            "foreach"
         ],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = ["++"],
      P.caseSensitive  = True
   }


data StringExpr
   = StrLit String


lexer = P.makeTokenParser languageDef

identifier  = P.identifier lexer

----------------------------------------------------------------------------------------------------
